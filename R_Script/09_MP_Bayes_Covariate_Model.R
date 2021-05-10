####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 09_MP_bayes_covariate_models

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory

#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")
setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )
library( rstan )
library( bayesplot )
library( rstanarm )

# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# Count data to get the domains and lat and long information
load("./Data/combinded.Rda")

# data structure for the daily data set

str(contigus.df)

str(complete.df)

#################### Combining data sets #############

# selecting the needed columns and data to merge with count data
cont.df <- contigus.df %>% dplyr::select( -c("Lat", "Long", "Date"))

# Joining data sets so that prism data is now linked to count data
full.df <- left_join(complete.df, cont.df, by=c("Plot","Year","DOY"))

## Number of rows should match complete.df of 620662
dim(full.df) # 620662 X 30

############ subsetting to the single species, site and year #########

# We are only interested in Aedes vexans from WOOD site
toy.df <- filter(full.df, SciName== "Aedes vexans" & Site == "WOOD")

unique(toy.df$Year)

## looking at the data
toy.df %>% 
  ggplot(aes(x=DOY,y=Count/TrapHours, color=Year)) +geom_point()



## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)

# lets start really simply and just focus on one year
toy.df <- toy.df %>% filter(Year == 2017)

## First model will be set up to fit a quadratic model that allows variation 
## in slope across years and random intercept for plot


full.m <- glmer( Count ~ poly(scale(CumGDD),2,raw=F)+offset(log(TrapHours))+
                   (1|Plot) + offset((TrapHours)), family="poisson",
                 data=toy.df,
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
summary(full.m)

## over dispersion check

performance::check_overdispersion(full.m) # Very over dispersed, need to model
# as a negative binomial or add an 
# observation random effect

# Creating the observation level factor

toy.df$Obs <- as.factor(1:nrow(toy.df))

full.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+
                   offset(log(TrapHours))+
                   (1|Plot) + (1|Obs),
                 family="poisson", data=toy.df,
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
summary(full.m)

full.m <- glmer( Count ~poly(scale(DOY),2,raw=F)* scale(Tmean7)+
                   poly(scale(DOY),2,raw=F)* scale(PPT14)+
                   offset(log(TrapHours))+
                   (1|Plot) + (1|Obs),
                 family="poisson", data=toy.df,
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
summary(full.m)


## over dispersion check

performance::check_overdispersion(full.m) # cool adding observation random
# effect helped 
##### Ok lets plot this

inter <- fixef(full.m)[1]
doy <- fixef(full.m)[2]
doy2 <- fixef(full.m)[3]

## getting dummy data frame
# getting the orthogonal polynomial terms
dum.df<- as.data.frame(poly(toy.df$DOY,2)[,1:2])

colnames(dum.df) <- c("sDOY", "sDOY2")

dum.df$DOY <- toy.df$DOY
dum.df$Pred <- NA

for( i in 1:nrow(dum.df)){
  dum.df$Pred[i] <- inter +
    dum.df$sDOY[i]*doy +
    dum.df$sDOY2[i]*doy2
}

# transform the predicted values to counts
dum.df$Pred.t <- exp(dum.df$Pred)

# plot the results
ggplot(dum.df,aes(x=DOY, y = Pred.t))+geom_line(size=2)+
  geom_point(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.5)

### Cool we now have a nice frequentist estimate of the polynomial patterns of 
### the data for 2017, now lets see how well we can capture this with a
## bayesian approach

poly.mat <- matrix( c(rep(1,nrow(toy.df)), poly(toy.df$DOY, 2)[,1], 
                      poly(toy.df$DOY, 2)[,2] ), ncol=3  )

## using samae data from before
poly.mat <- model.matrix(full.m)

plot.mat <- model.matrix(toy.df$Count ~ 0 + toy.df$Plot)

stan_d <- list( N= nrow(poly.mat), p = ncol(poly.mat), X = poly.mat,
                Plot = plot.mat, G = ncol(plot.mat),
                y=toy.df$Count, offset = toy.df$TrapHours)

multi_output <- stan( './R_Script/Stan_Models/MultilevelModel.stan',
                      data=stan_d, iter = 2000)


## print estimated coefficients
print(multi_output, pars = c("beta"))
traceplot(multi_output)

log_lik <- extract(multi_output)$lp__

lppd <- sum(log(mean(exp(log_lik))))



## lets plot the line of best fit
post <- rstan::extract(multi_output)

# getting the orthogonal polynomial terms
dum.df<- as.data.frame(model.matrix(simple.m))
plot.df <- as.data.frame(model.matrix(toy.df$Count ~ 0 + toy.df$Plot))
colnames(dum.df) <- c("intercept", "sDOY", "sDOY2")
dum.df$DOY <- toy.df$DOY
dum.df$Plot <- toy.df$Plot

t <- 1
for( i in 1:200){
  pred <- exp( post$beta[i,1] + post$beta[i,2]*dum.df$sDOY+ 
                 post$beta[i,3]*dum.df$sDOY2 +
                 post$alpha[i,1]*plot.df[,1]+
                 post$alpha[i,2]*plot.df[,2] + 
                 post$alpha[i,3]*plot.df[,3]+ 
                 post$alpha[i,4]*plot.df[,4]+ 
                 post$alpha[i,5]*plot.df[,5]+ 
                 post$alpha[i,6]*plot.df[,6]+ 
                 post$alpha[i,7]*plot.df[,7]+ 
                 post$alpha[i,8]*plot.df[,8]+ 
                 post$alpha[i,9]*plot.df[,9]+ 
                 post$alpha[i,10]*plot.df[,10] )
  dummy.df <- cbind.data.frame(dum.df$DOY,pred,dum.df$Plot)
  dummy.df$obs <- as.factor(i)
  if(t == 1 ){
    pred.df <- dummy.df
    t <- t +1
  }else{
    pred.df <- rbind.data.frame(pred.df, dummy.df)
  }
}

colnames(pred.df) <- c("DOY", "Pred", "Plot", "Obs")
pred.df$Obs <- as.character(pred.df$Obs)
pred.df$Grp <- paste(pred.df$Plot, "-", pred.df$Obs)
ggplot(pred.df, aes(x=DOY, y= Pred,  color=Plot))+ geom_line(aes(group=Grp),alpha=.1) +
  geom_point(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.7) +
  # facet_wrap(~Plot)+
  theme_classic() + ylab("Mosquito density")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


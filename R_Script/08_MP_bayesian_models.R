####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 08_MP_bayesian_models

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )
library( rstan )
library( bayesplot )

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
cont.df <- contigus.df %>% select( -c("Lat", "Long", "Date"))

# Joining datasets so that prism data is now linked to count data
full.df <- left_join(complete.df, cont.df, by=c("Plot","Year","DOY"))

## Number of rows should match complete.df of 620662
dim(full.df) # 620662 X 33

############ subsetting to the single species, site and year #########

# We are only intersted in Aedes vexans from WOOD site
toy.df <- filter(full.df, SciName== "Aedes vexans" & Site == "WOOD")

unique(toy.df$Year)

## summarize to DOY level
toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot, Year) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()

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


full.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+offset(log(TrapHours))+
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

full.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+offset(log(TrapHours))+
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

stan_d <- list( n= nrow(poly.mat), p = ncol(poly.mat), X = poly.mat,
                y=toy.df$Count, offset = toy.df$TrapHours)

output <- stan( './R_Script/Stan_Models/initModel.stan', data=stan_d)

## print estimated coefficients
print(output, pars = c("beta", "lp__"))

# lets compare to the simple glm version of the model
simple.m <- glm( Count ~ poly(scale(DOY),2,raw=F)+offset(log(TrapHours)),
                 family="poisson", data=toy.df)
summary(simple.m)

# checking the trace plot
traceplot(output)

# Plotting the coefficient
plot(output)

pairs(output)


## lets plot the line of best fit
post <- rstan::extract(output)

# getting the orthogonal polynomial terms
dum.df<- as.data.frame(poly(toy.df$DOY,2)[,1:2])

colnames(dum.df) <- c("sDOY", "sDOY2")

dum.df$DOY <- toy.df$DOY

n_iter <- length(post$lp__)

t <- 1
for( i in 1:n_iter){
  pred <- exp( post$beta[i,1] + post$beta[i,2]*dum.df$sDOY+ 
                 post$beta[i,3]*dum.df$sDOY2)
  dummy.df <- cbind.data.frame(dum.df$DOY,pred)
  dummy.df$obs <- as.factor(i)
  if(t == 1 ){
    pred.df <- dummy.df
    t <- t +1
  }else{
    pred.df <- rbind.data.frame(pred.df, dummy.df)
  }
}

colnames(pred.df) <- c("DOY", "Pred", "Obs")

ggplot(pred.df, aes(x=DOY, y= Pred, group=Obs))+ geom_line(alpha=.2,color="dark blue")+
  geom_jittwe(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.5)
  

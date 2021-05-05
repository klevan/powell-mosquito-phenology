####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 08_MP_bayesian_models

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
dim(full.df) # 620662 X 33

############ subsetting to the single species, site and year #########

# We are only interested in Aedes vexans from WOOD site
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

full.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+
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
class(poly.mat)


stan_d <- list( N= nrow(poly.mat), p = ncol(poly.mat), X = poly.mat,
                y=toy.df$Count, offset = toy.df$TrapHours)


init_output <- stan( './R_Script/Stan_Models/initModel.stan', data=stan_d, iter = 4000)

## print estimated coefficients
print(inti_output, pars = c("beta", "lp__"))

# lets compare to the simple glm version of the model
simple.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+offset(log(TrapHours))+
                     (1|Obs),
                 family="poisson", data=toy.df)
summary(simple.m)

# checking the trace plot
traceplot(init_output)

# Plotting the coefficient
plot(init_output)

pairs(init_output)


## lets plot the line of best fit
post <- rstan::extract(init_output)

# getting the orthogonal polynomial terms
dum.df<- as.data.frame(model.matrix(simple.m))

colnames(dum.df) <- c("intercept", "sDOY", "sDOY2")

dum.df$DOY <- toy.df$DOY

t <- 1
for( i in 1:100){
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
pred.df$Obs <- as.character(pred.df$Obs)
ggplot(pred.df, aes(x=DOY, y= Pred, group=Obs))+ geom_line(alpha=.1,color="blue") +
  geom_point(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.7) +
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
   
# While the model captures the simple pattern of increase mosquito density
# during the summer season and decrease later in the season however it fails
# to capture the magnitude and the multiple peaks, pretty sure the multiple
# peaks lets try a different model structure to see if it helps

# Model options:
#               1) Hurdle model: Model the occurrence and abundance as two
#                  separate processes
#               2) Auto regressive model: account for the temporal auto-correlation
#               3) adjust the random model structure to account for variation
#                  in both slope and intercept based on plot 

## random intercept


## using same data from before
poly.mat <- matrix( c(rep(1,nrow(toy.df)), poly(toy.df$DOY, 2)[,1], 
                      poly(toy.df$DOY, 2)[,2]) , ncol=3  )
plot.mat <- model.matrix(toy.df$Count ~ 0 + toy.df$Plot)

stan_d <- list( N= nrow(poly.mat), p = ncol(poly.mat), X = poly.mat,
                Plot = plot.mat, G = ncol(plot.mat),
                y=toy.df$Count, offset = toy.df$TrapHours)

multi_output <- stan( './R_Script/Stan_Models/MultilevelModel.stan',
                data=stan_d, iter = 2000)

## print estimated coefficients
print(multi_output, pars = c("alpha"))
traceplot(multi_output)

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
  #facet_wrap(~Plot)+
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


## Hurdle mode 

## using same data from before
poly.mat <- matrix( c(rep(1,nrow(toy.df)), poly(toy.df$DOY, 2)[,1], 
                      poly(toy.df$DOY, 2)[,2]) , ncol=3  )
plot.mat <- model.matrix(toy.df$Count ~ 0 + toy.df$Plot)

stan_d <- list( N= nrow(poly.mat), p = ncol(poly.mat), X = poly.mat,
                Plot = plot.mat, G = ncol(plot.mat),
                y=toy.df$Count, offset = toy.df$TrapHours)

Hurdle_output <- stan( './R_Script/Stan_Models/SimpleHurdle.stan',
                data=stan_d, iter = 4000)

## print estimated coefficients
print(Hurdle_output, pars = c("theta", "lambda"))
traceplot(Hurdle_output)


## print estimated coefficients
print(Hurdle_output, pars = c("alpha_poisson"))
traceplot(output)

#save(post, file="hurdle_output.rmd")

## lets plot the line of best fit
post <- rstan::extract(Hurdle_output)

## only the poisson component of the hurdle model

# getting the orthogonal polynomial terms
dum.df<- as.data.frame(model.matrix(simple.m))
plot.df <- as.data.frame(model.matrix(toy.df$Count ~ 0 + toy.df$Plot))
colnames(dum.df) <- c("intercept", "sDOY", "sDOY2")
dum.df$DOY <- toy.df$DOY
dum.df$Plot <- toy.df$Plot

t <- 1
for( i in 1:200){
  pred <- exp( post$lambda[i,1] + post$lambda[i,2]*dum.df$sDOY+ 
                 post$lambda[i,3]*dum.df$sDOY2 +
                 post$alpha_poisson[i,1]*plot.df[,1]+
                 post$alpha_poisson[i,2]*plot.df[,2] + 
                 post$alpha_poisson[i,3]*plot.df[,3]+ 
                 post$alpha_poisson[i,4]*plot.df[,4]+ 
                 post$alpha_poisson[i,5]*plot.df[,5]+ 
                 post$alpha_poisson[i,6]*plot.df[,6]+ 
                 post$alpha_poisson[i,7]*plot.df[,7]+ 
                 post$alpha_poisson[i,8]*plot.df[,8]+ 
                 post$alpha_poisson[i,9]*plot.df[,9]+ 
                 post$alpha_poisson[i,10]*plot.df[,10] ) 
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


## only the bernoulli component of the hurdle model
dum.df<- as.data.frame(model.matrix(simple.m))
plot.df <- as.data.frame(model.matrix(toy.df$Count ~ 0 + toy.df$Plot))
colnames(dum.df) <- c("intercept", "sDOY", "sDOY2")
dum.df$DOY <- toy.df$DOY
dum.df$Plot <- toy.df$Plot

t <- 1
for( i in 1:200){
  pred <- 1-plogis( post$theta[i,1] + post$theta[i,2]*dum.df$sDOY+ 
                 post$theta[i,3]*dum.df$sDOY2 +
                 post$alpha_bern[i,1]*plot.df[,1]+
                 post$alpha_bern[i,2]*plot.df[,2] + 
                 post$alpha_bern[i,3]*plot.df[,3]+ 
                 post$alpha_bern[i,4]*plot.df[,4]+ 
                 post$alpha_bern[i,5]*plot.df[,5]+ 
                 post$alpha_bern[i,6]*plot.df[,6]+ 
                 post$alpha_bern[i,7]*plot.df[,7]+ 
                 post$alpha_bern[i,8]*plot.df[,8]+ 
                 post$alpha_bern[i,9]*plot.df[,9]+ 
                 post$alpha_bern[i,10]*plot.df[,10] )
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

toy.df$MosPA <- NA
toy.df$MosPA[toy.df$Count >0 ] <- 1
toy.df$MosPA[toy.df$Count ==0 ] <- 0

ggplot(pred.df, aes(x=DOY, y= Pred,  color=Plot))+ geom_line(aes(group=Grp),
                                                             alpha=.1) +
  geom_point(data=toy.df, aes(x=DOY,y=MosPA),size=2, alpha=.7) +
  # facet_wrap(~Plot)+
  theme_classic() + ylab("Mosquito detection")+
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

### combining for the full model

## only the poisson component of the hurdle model

# getting the orthogonal polynomial terms
dum.df<- as.data.frame(model.matrix(simple.m))
plot.df <- as.data.frame(model.matrix(toy.df$Count ~ 0 + toy.df$Plot))
colnames(dum.df) <- c("intercept", "sDOY", "sDOY2")
dum.df$DOY <- toy.df$DOY
dum.df$Plot <- toy.df$Plot

t <- 1
for( i in 1:200){
  pred <- exp( post$lambda[i,1] + post$lambda[i,2]*dum.df$sDOY+ 
                 post$lambda[i,3]*dum.df$sDOY2 +
                 post$alpha_poisson[i,1]*plot.df[,1]+
                 post$alpha_poisson[i,2]*plot.df[,2] + 
                 post$alpha_poisson[i,3]*plot.df[,3]+ 
                 post$alpha_poisson[i,4]*plot.df[,4]+ 
                 post$alpha_poisson[i,5]*plot.df[,5]+ 
                 post$alpha_poisson[i,6]*plot.df[,6]+ 
                 post$alpha_poisson[i,7]*plot.df[,7]+ 
                 post$alpha_poisson[i,8]*plot.df[,8]+ 
                 post$alpha_poisson[i,9]*plot.df[,9]+ 
                 post$alpha_poisson[i,10]*plot.df[,10] ) *
    (1-plogis( post$theta[i,1] + post$theta[i,2]*dum.df$sDOY+ 
                post$theta[i,3]*dum.df$sDOY2 +
                post$alpha_bern[i,1]*plot.df[,1]+
                post$alpha_bern[i,2]*plot.df[,2]+ 
                post$alpha_bern[i,3]*plot.df[,3]+ 
                post$alpha_bern[i,4]*plot.df[,4]+ 
                post$alpha_bern[i,5]*plot.df[,5]+ 
                post$alpha_bern[i,6]*plot.df[,6]+ 
                post$alpha_bern[i,7]*plot.df[,7]+ 
                post$alpha_bern[i,8]*plot.df[,8]+ 
                post$alpha_bern[i,9]*plot.df[,9]+ 
                post$alpha_bern[i,10]*plot.df[,10] ))
  dummy.df <- cbind.data.frame(dum.df$DOY,pred,dum.df$Plot)
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



###### Cool lets move on to the autoregressive
 



## what if we wanted to model across different years

# Lets expand our toy data frame
toy.df <- filter(full.df, SciName== "Aedes vexans" & Site == "WOOD")

unique(toy.df$Year)

## summarize to DOY level
toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot, Year) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Obs <- as.character(1:nrow(toy.df))
# lets compare to the simple glm version of the model
simple.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)*fYear+offset(log(TrapHours))+
                     (1|Obs),
                 family="poisson", data=toy.df)
summary(simple.m)

DoyYear.mat <- model.matrix(simple.m)



stan_d <- list( n= nrow(DoyYear.mat), p = ncol(DoyYear.mat),
                X = DoyYear.mat,
                y=toy.df$Count, offset = toy.df$TrapHours)

output <- stan('./R_Script/Stan_Models/initModel.stan', data=stan_d, iter = 6000)

## print estimated coefficients
print(output, pars = c("beta", "lp__"))
plot(output)

traceplot(output)


#simple.m <-stan_glmer( Count ~ poly(scale(DOY),2,raw=F)*fYear+offset(log(TrapHours))+
                   #  (1|Obs),
                 #  family="poisson", data=toy.df)
#summary(simple.m)

#fit <- as.matrix(simple.m)
#ppc_dens_overlay( y= log10( (toy.df$Count/toy.df$TrapHours)+1),
                  #yrep= log10(posterior_predict(simple.m,draws=100)+1))


#ggplot(aes(x=toy.df$DOY, y=posterior_predict(simple.m,draws=100)))

post<- rstan::extract(output)


# getting the orthogonal polynomial terms
dum.df<- as.data.frame( DoyYear.mat )

colnames(dum.df) <- c("Intercept",'DOY',"DOY2", "y2016", "y2017",
                      "y2018","y2019", "DOY16", "DOY216", "DOY17",
                      "DOY217", "DOY18","DOY218", "DOY19", "DOY219")

dum.df$DOY <- toy.df$DOY
dum.df$fYear <- as.character(toy.df$Year)

n_iter <- length(post$lp__)

t <- 1
for( i in 5000:5050){
  pred <- exp( post$beta[i,1] + post$beta[i,2]*dum.df$DOY+ 
                 post$beta[i,3]*dum.df$DOY2+ post$beta[i,4]*dum.df$y2016+ 
                 post$beta[i,5]*dum.df$y2017+ post$beta[i,6]*dum.df$y2018+ 
                 post$beta[i,7]*dum.df$y2019+ post$beta[i,8]*dum.df$DOY16+ 
                 post$beta[i,9]*dum.df$DOY216+ post$beta[i,10]*dum.df$DOY17+ 
                 post$beta[i,11]*dum.df$DOY217+ post$beta[i,12]*dum.df$DOY18+ 
                 post$beta[i,13]*dum.df$DOY218+ post$beta[i,14]*dum.df$DOY19+ 
                 post$beta[i,15]*dum.df$DOY219)
  dummy.df <- cbind.data.frame(dum.df$DOY, dum.df$fYear,pred)
  dummy.df$obs <- as.factor(i)
  if(t == 1 ){
    pred.df <- dummy.df
    t <- t +1
  }else{
    pred.df <- rbind.data.frame(pred.df, dummy.df)
  }
}

colnames(pred.df) <- c("DOY", "fYear","Pred",   "Obs")
pred.df$Obs <- as.character(pred.df$Obs)
ggplot(pred.df, aes(x=DOY, y= Pred, color=as.factor(fYear),group=Obs))+ 
  geom_line(alpha=.1) +
  geom_point(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.7)




###### Quickly doing a gam model to  compare
library(mgcv)

# Lets expand our toy data frame
toy.df <- filter(full.df, SciName== "Aedes vexans" & Site == "WOOD")

unique(toy.df$Year)

## summarize to DOY level
toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot, Year) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Obs <- as.character(1:nrow(toy.df))

toy.df <- toy.df %>% filter(Year ==2017)


gam1 <- gam(Count ~ s(DOY) + offset(log(TrapHours)), family=poisson(link="log"),
            data=toy.df, method= "REML", knots =list(DOY=c(88,298)) )

gam.df <- data.frame( DOY = as.integer(88:298))

gam.df$TrapHours <- 24
ggplot(gam.df, aes(x=DOY, y= Fit/TrapHours))+ geom_line(size=2,color="blue") +
  geom_point(data=toy.df, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.7) +
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
gam.df$Fit <- predict(gam1, gam.df, type="response")



## checking overdispersion

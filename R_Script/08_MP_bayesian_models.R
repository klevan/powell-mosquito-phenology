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

## First model will be set up to fit a quadratic model that allows variation 
## in slope across years and random intercept for plot

full.m <- glmer( Count ~ scale(DOY)*as.factor(Year) + scale(DOY^2)*as.factor(Year) +
                   (1|Plot) + offset((TrapHours)), family="poisson",
                 data=toy.df,
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
summary(full.m)

performance::check_overdispersion(full.m) # Very overdispered, need to model
# as a negative binomial

toy.df$Obs <- as.factor(1:nrow(toy.df))

at1 <- toy.df %>% filter(Year == 2016)


full.m <- glmer( Count ~ poly(scale(DOY),2,raw=F)+offset(log(TrapHours))+
                   (1|Plot) + (1|Obs),
                 family="poisson", data=at1,
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl = list(maxfun=2e5)))
summary(full.m)

performance::check_overdispersion(full.m) # Very overdispered, need to model
# as a negative binomial

##### Ok lets plot this

inter <- fixef(full.m)[1]
doy <- fixef(full.m)[2]
doy2 <- fixef(full.m)[3]
## getting dumming dataframe


dum.df<- as.data.frame(poly(at1$DOY,2)[,1:2])


colnames(dum.df) <- c("sDOY", "sDOY2")

dum.df$DOY <- at1$DOY
dum.df$Pred <- NA

for( i in 1:nrow(dum.df)){
  dum.df$Pred[i] <- inter +
                    dum.df$sDOY[i]*doy +
                    dum.df$sDOY2[i]*doy2
}

dum.df$Pred.t <- exp(dum.df$Pred)

ggplot(dum.df,aes(x=DOY, y = Pred.t))+geom_line(size=2)+
  geom_point(data=at1, aes(x=DOY,y=Count/TrapHours),size=2, alpha=.5)


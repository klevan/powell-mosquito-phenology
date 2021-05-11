####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 05/10/2021
# title: 08a_MP_Autoregressive_Model

# The goal of the following script is to build an autogressive model for \
# mosquito abundance, there was some difficulties with this in bayesian 
# framework so lets try and do it frequentist

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

## summarize to DOY level
toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot, Year, Tmean7, PPT14, CumGDD) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()

## looking at the data
toy.df %>% 
  ggplot(aes(x=DOY,y=Count/TrapHours, color=Year)) +geom_point()



## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)
library(glmmTMB)
# lets start really simply and just focus on one year
toy.df <- toy.df %>% filter(Year == 2017)

## First model will be set up to fit a quadratic model that allows variation 
## in slope across years and random intercept for plot


toy.df$fTime <- as.factor(toy.df$DOY)

## building the first attempt

ar1.m <- glmmTMB(Count ~ gau(fTime + offset(log(TrapHours)) +  0 |Plot ), 
                             data=toy.df, family="poisson" )


summary(ar1.m)

new.data <- select(toy.df, c("fTime", "TrapHours", "Plot", "DOY"))                 

new.data <- unique(new.data)
new.data$Pred <- predict(ar1.m, newdata = new.data)


ggplot(new.data, aes(x=DOY, y= exp(Pred)/TrapHours, color=Plot)) +
  geom_line()+ geom_point(data=toy.df,aes(x=DOY, y= Count/TrapHours,color=Plot))+
  facet_wrap(~Plot)




ar1.temp.m <- glmmTMB(Count ~ toep(fTime + offset(log(TrapHours))  + 
                                    0|Plot ), 
                       data=toy.df, family="poisson" )




summary(ar1.temp.m)

new.data <- select(toy.df, c("fTime", "TrapHours", "Plot", "Tmean7", "DOY"))                 

new.data$Pred <- predict(ar1.m, newdata = new.data)


ggplot(new.data, aes(x=DOY, y= exp(Pred)/TrapHours, color=Plot)) +
  geom_line()+ geom_point(data=toy.df,aes(x=DOY, y= Count/TrapHours,color=Plot))+
  facet_wrap(~Plot)

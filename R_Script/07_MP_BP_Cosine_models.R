####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 07_MP_BP_Cosine_models

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )

# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# Count data to get the domains and lat and long information
load("./Data/combinded.Rda")


# data structure for the daily data set

str(contigus.df)

str(complete.df)

#################### Combinding data sets #############

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

## lets first just estimate for one year, 2017 was a good year for me so 

toy.year.df <- filter(toy.df, Year == 2017)

## summarize to DOY leve
toy.year.df <- toy.year.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY, Plot) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()
  
## Plot data
toy.year.df %>% 
ggplot( aes(x = DOY, y=Count/TrapHours, color=Plot))+geom_point()

#### building functions for the birth pulse model ####

# Estimate the number of emerging mosquitos across the year

# Parameters
# k = scaling factor propotional to the annual per capita birth rate
# t = the time steps will be the DOY
# s = birthing synchrony, how wide and tall the birthing peak is
# phi = the timing of the differnt pulses

bp <-function(k ,s, phi,t){
  # vector to store my predicted abudance patterns
  pred_bp <- rep(NA, t)
  
  # Looping through the time steps to predict abundance
  for( i in 1:t){
    time <- i/t
    pred_bp[i] <- k * sqrt(s/pi) * exp( -s * cos( (pi*time) * (phi) )^2 )
  }
  # returning the vector of predictions
  return( pred_bp )
}


at <- bp(k =0.0041,s =2, phi = 2 ,t= 300)


plot(y=at, x= 1:300)



#### Cosine function 

# Key parameters
# phi = number of birthing events
# k = scaler for max population size

cosinePop <-function(k, phi,t){
  # vector to store my predicted abudance patterns
  pred_cosine <- rep(NA, t)
  
  # Looping through the time steps to predict abundance
  for( i in 1:t){
    time <- i/t
    pred_cosine[i] <- k* cos( (pi*time) * (phi) )^2 
  }
  # returning the vector of predictions
  return( pred_cosine )
}


at <-  cosinePop(k=100,phi = 2 ,t= 300)


plot(y=at, x= 1:300)


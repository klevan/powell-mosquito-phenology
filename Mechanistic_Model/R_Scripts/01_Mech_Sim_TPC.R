####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 01_Mech_Sim_TPC

# The goal of the following script is to code thermal performance curves (TPC)
# for the differnt mosquito thermal sensitve traits ( Development, morality,
# and reproduction)

## The TPC function comes from Childress and Letcher 2017 - Ecology
## Estimated parameters (Toptim and CTmax) estimated based on reported values
## from Mordecai et al. 2019

#Set working directory
#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

#setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

#### Building a thermal performance curve for mosquito deveopment rate

Dev_TPC <- function( Temp, Toptim, CTmax, sigma){
  
  perform <- rep(NA, length(Temp)) # Creating a vector of trait value based on
                                   # given temperature
  
  for( i in 1:length(Temp)){ # Looping through each temperature value
    if( Temp[i] <= Toptim){ # If temperature is less than or equal to Toptim
      perform[i] <- exp(1)^( (Temp[i] - Toptim)/ (2 * (sigma^2))  ) 
    }else{ # if temperature is greater than the optim temp
      perform[i] <- 1 - ( (Temp[i] - Toptim) / (Toptim - CTmax) )^2
    }
  } 
  
return(perform)
    
}


temp <- seq(0, 20, length.out = 100)
at1 <- Dev_TPC( Temp = temp, Toptim = 14, CTmax= 25, sigma = 2.5)

plot(x= temp, y= at1/5)


### Extracting temp data ###

## setting wd to upload data
setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )
library(gamm4)
# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# Count data to get the domains and lat and long information
load("./Data/combinded.Rda")

#################### modeling climate patterns across plot

site.df <- unique(select(ungroup(complete.df), c("Site", "Plot")))



contig.df <- left_join(contigus.df, site.df , by="Plot")


focal.df <- filter( contig.df, Site=="WOOD")

focal.df$fYear <- as.factor(focal.df$Year)

### Temperature
gam.temp <- gamm4( Tmean7 ~ s(DOY,by=(fYear), bs="cc"),
                   random = ~ (1|Plot),
                   data=focal.df, family="gaussian")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 0:365
fYear <- c(levels(focal.df$fYear))
plot.m <- c(levels(as.factor(focal.df$Plot)))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)


dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),plot.m)

dum.df$Pred <- predict(gam.temp$gam, newdata = dum.df)


dum.df %>% filter( fYear != "2013") %>% 
  ggplot(  aes(x=DOY, y=(Pred)))+ 
  geom_point(data=focal.df, aes(x= DOY,y=Tmean7, color=fYear),size=2)+
  #geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75,color="black")+ theme_classic()+ 
  facet_wrap(~fYear)


temp.df <- dum.df

colnames(temp.df) <- c("DOY", "fYear", "Plot", "Pred")

temp.df$Metric <- "Temp"

pred.df <- select(temp.df, c("DOY", "Pred", "fYear"))

pred.df <- unique(pred.df %>% filter(fYear == "2017"))

colnames(pred.df)[2] <- "Temp"

pred.df$Juv <- NA
pred.df$Adult <- NA


## Initial parameters

pred.df$Juv[1] <- 100 ## overwinter larval population
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0

fec <- 1.2 ## setting a fecudity rate of 3 eggs per day per female
aMortal <- 0.25 ## setting mortality rate for adults
lMortal <- .18 ## setting mortality rate for larvae

## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## Larval dynamics
  pred.df$Juv[i+1] <- pred.df$Juv[i]+ (pred.df$Adult[i] * fec) -
                      pred.df$Juv[i] * lMortal - 
                      pred.df$Juv[i] * (Dev_TPC( pred.df$Temp[i], Toptim = 18,
                                                 CTmax=30, sigma = 2.5)/8)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - (pred.df$Adult[i] * aMortal) +
    pred.df$Juv[i] *  (Dev_TPC( Temp = pred.df$Temp[i], Toptim = 18, 
                                CTmax= 30, sigma = 2.5)/8)
}

pred.df %>% filter(DOY > 100) %>% 
ggplot( aes( x=DOY, y=Adult)) + geom_point() +
  geom_point( aes(x =DOY, y= Juv), color= "grey")

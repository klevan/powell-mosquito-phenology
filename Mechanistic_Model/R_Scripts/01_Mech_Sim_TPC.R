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

## testing the TPC function
temp <- seq(-10, 20, length.out = 100) # initial temperature range

at1 <- Dev_TPC( Temp = temp, Toptim = 18, CTmax= 30, sigma = 1.5)

# plotting the thermal performance curve

plot(x= temp, y= at1/3)
# doesn't look that good but we can carry on with it

### Extracting temp data from PRISM, use this for our seasonal model ###

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

# Need to add Site info to prism data 
contig.df <- left_join(contigus.df, site.df , by="Plot")

focal.df <- filter( contig.df, Site=="WOOD") # modeling wood from D9

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

# Looking at predicted temp data
dum.df %>% filter( fYear != "2013") %>% 
  ggplot(  aes(x=DOY, y=(Pred)))+ 
  geom_point(data=focal.df, aes(x= DOY,y=Tmean7, color=fYear),size=2)+
  #geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75,color="black")+ theme_classic()+ 
  facet_wrap(~fYear)

# putting predicted temp data to new data set
temp.df <- dum.df

# Adding improved col names
colnames(temp.df) <- c("DOY", "fYear", "Plot", "Pred")

# selecting col. of interest
pred.df <- select(temp.df, c("DOY", "Pred", "fYear"))

# filtering out so we just have one year of temp data
pred.df <- unique(pred.df %>% filter(fYear == "2017"))

# renaming column
colnames(pred.df)[2] <- "Temp"

# filtering to better start with when mosquitoes are present
pred.df <- pred.df %>%  filter(DOY>=100)

# adding columns to track JUV, and adults
pred.df$Juv <- NA
pred.df$Adult <- NA

# column to track changes in development rate
pred.df$DevRate <-NA ## tracking proportion of larvae the develop into adults
                     ## per day, sensitive to the temperature

## Initial parameters

pred.df$Juv[1] <- 1000 ## overwinter larval population, just throwing an initial
                       ## starting value out (based on nothing)
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0, throwing an initial
                      ## starting value out (based on nothing)


fec <- 1.02 ## setting a fecundity rate of 3 eggs per day per female
            ## number of eggs laid per day,
aMortal <- 0.25 ## setting mortality rate for adults
                ## proportion of Adults that die per day (24 hr)
lMortal <- .2 ## setting mortality rate for larvae
              ## propotion of larvae that die per day (24 hr)


## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## Larval dynamics
  pred.df$Juv[i+1] <- pred.df$Juv[i]+ (pred.df$Adult[i] * fec) -
                      pred.df$Juv[i] * lMortal - 
                      pred.df$Juv[i] * (Dev_TPC( pred.df$Temp[i],  Toptim =15,
                                                 CTmax= 25, sigma = 1.5)/2)
  pred.df$DevRate[i] <- Dev_TPC( pred.df$Temp[i],  Toptim = 15,
                                 CTmax=25, sigma = 1.5)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - (pred.df$Adult[i] * aMortal) +
    pred.df$Juv[i] *  (Dev_TPC( Temp = pred.df$Temp[i], Toptim = 15, 
                                CTmax= 25, sigma = 1.5)/4)
}

pred.df %>% filter(DOY >= 150) %>% 
ggplot( aes( x=DOY, y=Adult)) + geom_line(size=2, alpha=.85,
                                          aes(color="black")) +
  geom_line(aes(x=DOY, y= Juv, color="grey"), size=2,alpha=.75) +
  ylab("Mosquito abundance") + theme_classic()+
  scale_color_manual(name="Stage", values=c("black" = "black","grey"="grey"),
                     labels=c("Adult", "Larvae"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.9),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



pred.df %>% 
  ggplot( aes( x=DOY, y=DevRate)) + geom_line(size=2, alpha=.85,
                                              aes(color="black")) +
  geom_line(aes(x=DOY, y= Temp/max(Temp), color="grey"), size=2,alpha=.75) +
  scale_y_continuous( sec.axis = sec_axis(~. * max(pred.df$Temp),
                                          name="Mean Temp (C)"),
                      name="Relative Development Rate") + theme_classic()+
  scale_color_manual(name="", values=c("black" = "black","grey"="grey"),
                     labels=c("Devel. rate", "Temp."))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.9),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

             
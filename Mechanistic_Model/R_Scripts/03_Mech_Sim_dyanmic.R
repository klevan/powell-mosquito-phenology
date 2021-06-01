####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
#  05/21/2021
# title: 03_Mech_Sim_dyanmic

# The following script is to use the model built in previous code
# (01_Mech_Sim_TPC.R) and incorporate additional levels of temperature sensitive
# parameters (mortality and fecudity)

## The TPC function comes from Childress and Letcher 2017 - Ecology
## Estimated parameters (Toptim and CTmax) estimated based on reported values
## from Mordecai et al. 2019


#### Building a thermal performance curve for the different themeral sensitive
#### Traits

TPC <- function( Temp, Toptim, CTmax, sigma){
  
  perform <- rep(NA, length(Temp)) # Creating a vector of trait value based on
  # given temperature
  
  for( i in 1:length(Temp)){ # Looping through each temperature value
    if( Temp[i] <= Toptim){ # If temperature is less than or equal to Toptim
      perform[i] <- exp(1)^( (Temp[i] - Toptim)/ (2 *sigma)^2  ) 
    }else{ # if temperature is greater than the optim temp
      perform[i] <- 1 - ( (Temp[i] - Toptim) / (Toptim - CTmax) )^2
    }
  } 
  
  perform[perform < 0] <- 0 
  return(perform)
  
}

## testing the TPC function
temp <- seq(-5, 45, length.out = 100) # initial temperature range

at1 <- TPC( Temp = temp, Toptim = 25, CTmax= 40, sigma = 1.2)

# plotting the thermal performance curve

plot(x= temp, y= at1)
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

summary(gam.temp$gam)
## lets try and plot this with new data

DOY <- 0:365
fYear <- c(levels(focal.df$fYear))
plot.m <- c(levels(as.factor(focal.df$Plot)))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)

dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),plot.m)

dum.df$Pred <- predict(gam.temp$gam, newdata = dum.df)

# Looking at predicted number
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
pred.df$Juv1 <- NA
pred.df$Juv2 <- NA
pred.df$Juv3 <- NA
pred.df$Adult <- NA

## Inital parameters

pred.df$DevRate <-NA ## tracking proportion of larvae the develop into adults
                     ## per day, sensitive to the temperature
pred.df$FecRate <- NA ## tracking the number of eggs laid per female adult mos
                      ## per day, sensitive to the temperature
pred.df$aMortRate <- NA ## tracking proportion adult mosquitoes that die per day
                        ## per day, sensitive to the temperature
pred.df$lMortRate <- NA ##  tracking proportion of larvae that die per day
                        ## per day, sensitive to the temperature

# Starting values
pred.df$Juv1[1] <- 1000## overwinter larval population
pred.df$Juv2[1] <- 1000## overwinter larval population
pred.df$Juv3[1] <- 1000 ## overwinter larval population
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0

## Fixed parameters

fec <- 2.4 ## setting a fecundity rate of 3 eggs per day per female
## number of eggs laid per day,
aMortal <- 0.35 ## setting mortality rate for adults
## proportion of Adults that die per day (24 hr)
lMortal <- .3 ## setting mortality rate for larvae
## proportion of larvae that die per day (24 hr)


## Lets run the model

## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## Larval dynamics
  pred.df$Juv1[i+1] <- pred.df$Juv1[i]+ (pred.df$Adult[i] * fec) -
    pred.df$Juv1[i] * lMortal - 
    pred.df$Juv1[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                                CTmax= 25, sigma = 1.5)/2)
  
  pred.df$Juv2[i+1] <- pred.df$Juv2[i]+ (pred.df$Juv1[i] * 
                                           (TPC( pred.df$Temp[i],  Toptim =20,
                                                     
                                                     CTmax= 25, sigma = 1.5)/2)) -
    pred.df$Juv2[i] * lMortal - 
    pred.df$Juv2[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                                CTmax= 25, sigma = 1.5)/2)
  
  pred.df$Juv3[i+1] <- pred.df$Juv3[i]+ (pred.df$Juv2[i] * 
                                           (TPC( pred.df$Temp[i],  Toptim =20,
                                                     
                                                     CTmax= 25, sigma = 1.5)/2)) -
    pred.df$Juv3[i] * lMortal - 
    pred.df$Juv3[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                                CTmax= 25, sigma = 1.5)/2)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - (pred.df$Adult[i] * aMortal) +
    pred.df$Juv3[i] *  (TPC( Temp = pred.df$Temp[i], Toptim = 20, 
                                 CTmax= 25, sigma = 1.5)/2)
  # tracking the development rate across time
  pred.df$DevRate[i] <- TPC( pred.df$Temp[i],  Toptim = 20 ,
                                 CTmax=25, sigma = 1.5)
}

pred.df %>% filter(DOY >= 150) %>% 
  ggplot( aes( x=DOY, y=(Adult))) + geom_line(size=2, alpha=.85,
                                              aes(color="black")) +
  geom_line(aes(x=DOY, y= (Juv1), color="blue"), size=2,alpha=.75) +
  geom_line(aes(x=DOY, y= (Juv2), color="green"), size=2,alpha=.75) +
  geom_line(aes(x=DOY, y= (Juv3), color="red"), size=2,alpha=.75) +
  ylab("Mosquito abundance") + theme_classic() +
  scale_color_manual(name="Stage", values=c("black" = "#003f5c",
                                            "blue"="#7a5195",
                                            "green"= '#ef5675',
                                            "red" = "#ffa600"),
                     labels=c("Adult", "Early instar", "Late instar",
                              "Pupa"))+
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
                     labels=c("Devel. rate", "Temp."))+# selecting col. of interest
  pred.df <- select(temp.df, c("DOY", "Pred", "fYear"))

# filtering out so we just have one year of temp data
pred.df <- unique(pred.df %>% filter(fYear == "2017"))

# renaming column
colnames(pred.df)[2] <- "Temp"

# filtering to better start with when mosquitoes are present
pred.df <- pred.df %>%  filter(DOY>=100)

# adding columns to track JUV, and adults
pred.df$Juv1 <- NA
pred.df$Juv2 <- NA
pred.df$Juv3 <- NA
pred.df$Adult <- NA

## Inital parameters

pred.df$DevRate <-NA ## tracking proportion of larvae the develop into adults
## per day, sensitive to the temperature
pred.df$FecRate <- NA ## tracking the number of eggs laid per female adult mos
## per day, sensitive to the temperature
pred.df$aMortRate <- NA ## tracking proportion adult mosquitoes that die per day
## per day, sensitive to the temperature
pred.df$lMortRate <- NA ##  tracking proportion of larvae that die per day
## per day, sensitive to the temperature

# Starting values
pred.df$Juv1[1] <- 1000## overwinter larval population
pred.df$Juv2[1] <- 1000## overwinter larval population
pred.df$Juv3[1] <- 1000 ## overwinter larval population
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0

## Fixed parameters

fec <- 2.4 ## setting a fecundity rate of 3 eggs per day per female
## number of eggs laid per day,
aMortal <- 0.35 ## setting mortality rate for adults
## proportion of Adults that die per day (24 hr)
lMortal <- .3 ## setting mortality rate for larvae
## proportion of larvae that die per day (24 hr)


## Lets run the model

## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## Larval dynamics
  pred.df$Juv1[i+1] <- pred.df$Juv1[i]+ (pred.df$Adult[i] * fec) -
    pred.df$Juv1[i] * lMortal - 
    pred.df$Juv1[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                            CTmax= 25, sigma = 1.5)/2)
  
  pred.df$Juv2[i+1] <- pred.df$Juv2[i]+ (pred.df$Juv1[i] * 
                                           (TPC( pred.df$Temp[i],  Toptim =20,
                                                 
                                                 CTmax= 25, sigma = 1.5)/2)) -
    pred.df$Juv2[i] * lMortal - 
    pred.df$Juv2[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                            CTmax= 25, sigma = 1.5)/2)
  
  pred.df$Juv3[i+1] <- pred.df$Juv3[i]+ (pred.df$Juv2[i] * 
                                           (TPC( pred.df$Temp[i],  Toptim =20,
                                                 
                                                 CTmax= 25, sigma = 1.5)/2)) -
    pred.df$Juv3[i] * lMortal - 
    pred.df$Juv3[i] * (TPC( pred.df$Temp[i],  Toptim =20,
                            CTmax= 25, sigma = 1.5)/2)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - (pred.df$Adult[i] * aMortal) +
    pred.df$Juv3[i] *  (TPC( Temp = pred.df$Temp[i], Toptim = 20, 
                             CTmax= 25, sigma = 1.5)/2)
  # tracking the development rate across time
  pred.df$DevRate[i] <- TPC( pred.df$Temp[i],  Toptim = 20 ,
                             CTmax=25, sigma = 1.5)
}

pred.df %>% filter(DOY >= 150) %>% 
  ggplot( aes( x=DOY, y=(Adult))) + geom_line(size=2, alpha=.85,
                                              aes(color="black")) +
  geom_line(aes(x=DOY, y= (Juv1), color="blue"), size=2,alpha=.75) +
  geom_line(aes(x=DOY, y= (Juv2), color="green"), size=2,alpha=.75) +
  geom_line(aes(x=DOY, y= (Juv3), color="red"), size=2,alpha=.75) +
  ylab("Mosquito abundance") + theme_classic() +
  scale_color_manual(name="Stage", values=c("black" = "#003f5c",
                                            "blue"="#7a5195",
                                            "green"= '#ef5675',
                                            "red" = "#ffa600"),
                     labels=c("Adult", "Early instar", "Late instar",
                              "Pupa"))+
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


## Modeling temporal dynamic mortality , developmental , and fecudity rates


# selecting col. of interest
pred.df <- select(temp.df, c("DOY", "Pred", "fYear"))

# filtering out so we just have one year of temp data
pred.df <- unique(pred.df %>% filter(fYear == "2017"))

# renaming column
colnames(pred.df)[2] <- "Temp"

# filtering to better start with when mosquitoes are present
pred.df <- pred.df %>%  filter(DOY>=200)

# adding columns to track JUV, and adults
pred.df$Juv1 <- NA
pred.df$Juv2 <- NA
pred.df$Juv3 <- NA
pred.df$Adult <- NA

## Inital parameters

pred.df$DevRate <-NA ## tracking proportion of larvae the develop into adults
## per day, sensitive to the temperature
pred.df$FecRate <- NA ## tracking the number of eggs laid per female adult mos
## per day, sensitive to the temperature
pred.df$aMortRate <- NA ## tracking proportion adult mosquitoes that die per day
## per day, sensitive to the temperature
pred.df$lMortRate <- NA ##  tracking proportion of larvae that die per day
## per day, sensitive to the temperature

# Starting values
pred.df$Juv1[1] <- 10## overwinter larval population
pred.df$Juv2[1] <- 0## overwinter larval population
pred.df$Juv3[1] <- 0 ## overwinter larval population
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0

## Fixed baseline parameters

fec <- 12 ## setting a fecundity rate of 3 eggs per day per female
## number of eggs laid per day,
aMortal <- 0.7 ## setting mortality rate for adults
## proportion of Adults that die per day (24 hr)
lMortal <- .8 ## setting mortality rate for larvae
## proportion of larvae that die per day (24 hr)

# Specifying the TPC parameters for development, fecundity and mortality

## Development: 

dev_par <- c( 15,  # Toptim: temperature where trait value is maximized
              22,  # CTmax: critical threshold for temperature
              .75) # sigma: parameter that shapes slope as temp approaches Toptim

fec_par <- c( 37,  # Toptim: temperature where trait value is maximized
              39,  # CTmax: critical threshold for temperature
             2) # sigma: parameter that shapes slope as temp approaches Toptim

aMort_par <- c( 20,# Toptim: temperature where trait value is maximized
              30,  # CTmax: critical threshold for temperature
              .95) # sigma: parameter that shapes slope as temp approaches Toptim

lMort_par <- c( 20,# Toptim: temperature where trait value is maximized
              30,  # CTmax: critical threshold for temperature
              2.5) # sigma: parameter that shapes slope as temp approaches Toptim


## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## temperature sensitive parameters
  # tracking the development rate across time
  pred.df$DevRate[i] <- TPC( pred.df$Temp[i],
                             Toptim = dev_par[1],
                             CTmax= dev_par[2],
                             sigma = dev_par[3])
  
  pred.df$FecRate[i] <- TPC( pred.df$Temp[i],
                             Toptim = fec_par[1],
                             CTmax= fec_par[2],
                             sigma = fec_par[3])
  
  pred.df$aMortRate[i] <- 1 - TPC( pred.df$Temp[i],
                             Toptim = aMort_par[1],
                             CTmax= aMort_par[2],
                             sigma = aMort_par[3])
  
  
  pred.df$lMortRate[i] <- 1 - TPC( pred.df$Temp[i],
                             Toptim = lMort_par[1],
                             CTmax= lMort_par[2],
                             sigma = lMort_par[3])
  
  
  ## Discrete step model
  
  ## Larval dynamics
  pred.df$Juv1[i+1] <- pred.df$Juv1[i]+ (pred.df$Adult[i] * 
                                        ( fec* pred.df$FecRate[i]) ) -
                       pred.df$Juv1[i] * (lMortal * pred.df$lMortRate[i] ) - 
                       pred.df$Juv1[i] * ( pred.df$DevRate[i]/2)
  
  pred.df$Juv2[i+1] <- pred.df$Juv2[i]+ pred.df$Juv1[i] * 
                                        (pred.df$DevRate[i]/2) -
                       pred.df$Juv2[i] *  (lMortal * pred.df$lMortRate[i] ) - 
                       pred.df$Juv2[i] * ( pred.df$DevRate[i]/2)
    
  pred.df$Juv3[i+1] <- pred.df$Juv3[i]+ pred.df$Juv2[i] * 
                                           (pred.df$DevRate[i]/2) -
                       pred.df$Juv3[i] * (lMortal * pred.df$lMortRate[i] ) - 
                       pred.df$Juv3[i] * ( pred.df$DevRate[i]/2)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - pred.df$Adult[i] *
                                           (aMortal * pred.df$aMortRate[i]) +
                        pred.df$Juv3[i] *  ( pred.df$DevRate[i]/2)
  }
  

#### Just adults


pred.df %>% filter(DOY >= 200) %>% 
  ggplot( aes( x=DOY, y=(Adult))) + geom_line(size=2, alpha=.85,
                                              color="black") +
  #ylim(0,30)+
  ylab("Mosquito abundance") + theme_classic() +
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



pred.df %>% filter(DOY >= 200) %>% 
    ggplot( aes( x=DOY, y=(Adult))) + geom_line(size=2, alpha=.85,
                                                aes(color="black")) +
    geom_line(aes(x=DOY, y= (Juv1), color="blue"), size=2,alpha=.75) +
    geom_line(aes(x=DOY, y= (Juv2), color="green"), size=2,alpha=.75) +
    geom_line(aes(x=DOY, y= (Juv3), color="red"), size=2,alpha=.75) +
    ylab("Mosquito abundance") + theme_classic() +
    scale_color_manual(name="Stage", values=c("black" = "#003f5c",
                                              "blue"="#7a5195",
                                              "green"= '#ef5675',
                                              "red" = "#ffa600"),
                       labels=c("Adult", "Early instar", "Late instar",
                                "Pupa"))+
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
    geom_line(aes(x=DOY, y= FecRate, color="grey"), size=2,alpha=.75) +
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
  
  



### building a for loop to explore how parameters shape the modality

# building a dataset to track parameter values with modality outcome

tOptim_Range <- seq(15,40, length.out = 100)
sig_Range <- seq(.5,5,length.out = 100)

par.df <- as.data.frame(expand.grid(tOptim_Range ,sig_Range))

colnames(par.df) <- c("tOptim", 'Sigma')

par.df$Ctmax <- par.df$tOptim+2

par.df$Modal <- NA

par.df$Degree <- NA


for(r in 1:nrow(par.df)){
# selecting col. of interest
pred.df <- select(temp.df, c("DOY", "Pred", "fYear"))

# filtering out so we just have one year of temp data
pred.df <- unique(pred.df %>% filter(fYear == "2017"))

# renaming column
colnames(pred.df)[2] <- "Temp"

# filtering to better start with when mosquitoes are present
pred.df <- pred.df %>%  filter(DOY>=200)

# adding columns to track JUV, and adults
pred.df$Juv1 <- NA
pred.df$Juv2 <- NA
pred.df$Juv3 <- NA
pred.df$Adult <- NA

## Inital parameters

pred.df$DevRate <-NA ## tracking proportion of larvae the develop into adults
## per day, sensitive to the temperature
pred.df$FecRate <- NA ## tracking the number of eggs laid per female adult mos
## per day, sensitive to the temperature
pred.df$aMortRate <- NA ## tracking proportion adult mosquitoes that die per day
## per day, sensitive to the temperature
pred.df$lMortRate <- NA ##  tracking proportion of larvae that die per day
## per day, sensitive to the temperature

# Starting values
pred.df$Juv1[1] <- 10## overwinter larval population
pred.df$Juv2[1] <- 0## overwinter larval population
pred.df$Juv3[1] <- 0 ## overwinter larval population
pred.df$Adult[1] <- 0 ## number of adults in day 1 should be 0

## Fixed baseline parameters

fec <- 12 ## setting a fecundity rate of 3 eggs per day per female
## number of eggs laid per day,
aMortal <- 0.7 ## setting mortality rate for adults
## proportion of Adults that die per day (24 hr)
lMortal <- .8 ## setting mortality rate for larvae
## proportion of larvae that die per day (24 hr)

# Specifying the TPC parameters for development, fecundity and mortality

## Development: 

dev_par <- c( 15,  # Toptim: temperature where trait value is maximized
              22,  # CTmax: critical threshold for temperature
              .75) # sigma: parameter that shapes slope as temp approaches Toptim

aMort_par <- c( 25,# Toptim: temperature where trait value is maximized
                40,  # CTmax: critical threshold for temperature
                .95) # sigma: parameter that shapes slope as temp approaches Toptim

lMort_par <- c( 25,# Toptim: temperature where trait value is maximized
                40,  # CTmax: critical threshold for temperature
                2.5) # sigma: parameter that shapes slope as temp approaches Toptim


## Lets run the model

for( i in 1:(nrow(pred.df)-1)){
  ## temperature sensitive parameters
  # tracking the development rate across time
  pred.df$DevRate[i] <- TPC( pred.df$Temp[i],
                             Toptim = dev_par[1],
                             CTmax= dev_par[2],
                             sigma = dev_par[3])
  
  pred.df$FecRate[i] <- TPC( pred.df$Temp[i],
                             Toptim = par.df$tOptim[r],
                             CTmax= par.df$Ctmax[r],
                             sigma = par.df$Sigma[r])
  
  pred.df$aMortRate[i] <- 1 - TPC( pred.df$Temp[i],
                                   Toptim = aMort_par[1],
                                   CTmax= aMort_par[2],
                                   sigma = aMort_par[3])
  
  
  pred.df$lMortRate[i] <- 1 - TPC( pred.df$Temp[i],
                                   Toptim = lMort_par[1],
                                   CTmax= lMort_par[2],
                                   sigma = lMort_par[3])
  
  
  ## Discrete step model
  
  ## Larval dynamics
  pred.df$Juv1[i+1] <- pred.df$Juv1[i]+ (pred.df$Adult[i] * 
                                           ( fec* pred.df$FecRate[i]) ) -
    pred.df$Juv1[i] * (lMortal * pred.df$lMortRate[i] ) - 
    pred.df$Juv1[i] * ( pred.df$DevRate[i]/2)
  
  pred.df$Juv2[i+1] <- pred.df$Juv2[i]+ pred.df$Juv1[i] * 
    (pred.df$DevRate[i]/2) -
    pred.df$Juv2[i] *  (lMortal * pred.df$lMortRate[i] ) - 
    pred.df$Juv2[i] * ( pred.df$DevRate[i]/2)
  
  pred.df$Juv3[i+1] <- pred.df$Juv3[i]+ pred.df$Juv2[i] * 
    (pred.df$DevRate[i]/2) -
    pred.df$Juv3[i] * (lMortal * pred.df$lMortRate[i] ) - 
    pred.df$Juv3[i] * ( pred.df$DevRate[i]/2)
  ## Adult dynamics
  pred.df$Adult[i+1] <- pred.df$Adult[i] - pred.df$Adult[i] *
    (aMortal * pred.df$aMortRate[i]) +
    pred.df$Juv3[i] *  ( pred.df$DevRate[i]/2)

}


### sumarizing simulated data
test.df <- pred.df
test.df$rDirct <- NA
test.df$Delt <- NA

for( t in 2:nrow(test.df)){
  if( test.df$Adult[t] >= test.df$Adult[t-1]){
    test.df$rDirct[t] <- 1
  }else(
    test.df$rDirct[t] <- -1 
  )
  if( t > 2){
    test.df$Delt[t] <- test.df$rDirct[t]+ test.df$rDirct[t-1]
  }
}


sum.df <- filter(test.df, Delt == 0)
if( nrow(sum.df) == 1){
  par.df$Modal[r] <- 1 
}else( par.df$Modal[r] <- 2)
par.df$Degree[r]<- (sum.df$Adult[1] - sum.df$Adult[2] ) / sum.df$Adult[1]


}

par.df$Degree[par.df$Modal==1] <- 0
ggplot(par.df, aes(x=tOptim, y= Sigma))+ geom_raster(aes(fill=Degree)) + theme_classic()

ggplot(par.df, aes(x=tOptim, y= Sigma))+ geom_raster(aes(fill=)) + theme_classic()

pred.df %>% filter(DOY >= 200& DOY <=220) %>% 
  ggplot( aes( x=DOY, y=(Adult))) + geom_line(size=2, alpha=.85,
                                              color="black") +
  #ylim(0,30)+
  ylab("Mosquito abundance") + theme_classic() +
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

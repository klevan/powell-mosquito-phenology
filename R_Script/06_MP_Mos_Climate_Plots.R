####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/19/2021
# title: 06_MP_Mos_Climate_Plot

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

cont
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

############ Exploration of the combind data frames #########

# First step is to see what the PRISM data looks like at this sampling effort
# and identify any issues that may need to be resolved in previous scripts

## Tmax across time (Using the moving y day average)

full.df %>% filter(Year >2013 & Year < 2020) %>% 
  ggplot(aes(x=DOY, y= Tmean7, color=Domain)) + geom_point(size=2) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")

# Looks good, definatly captures the variation in seasonal temp fairly well

## Precipitaion patterns ( using the 14 day moving totals)

full.df %>% filter(Year >2013 & Year < 2020) %>% 
  ggplot(aes(x=DOY, y= PPT14, color=Domain)) + geom_point(size=2) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")

## Whoa this is all over the place, maybe it might make sense to use a total 
## precip, would have to identify what sites to use water year


## Precipitaion patterns ( using the 14 day moving total of rainy days)

full.df %>% filter(Year >2013 & Year < 2020) %>% 
  ggplot(aes(x=DOY, y= Rain14, color=Year)) + geom_point(size=2) +
  theme_classic() + facet_wrap(~Domain) + theme( legend.position = "None")

## looks pertty decent, though maybe would be good to do the reverse and look
## at the number of dry days or consequitive dry days

## GDD ( using the cumulative gdd)

full.df %>% filter(Year >2013 & Year < 2020) %>% 
  ggplot(aes(x=DOY, y= CumGDD, color=Domain)) + geom_point(size=2) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")

## This looks really good ! Lets match both this and temperature to mosquito 
## abundance

# lets just look at photoperiod for a sense of calm
full.df %>% filter(Year >2013 & Year < 2020) %>% 
  ggplot(aes(x=DOY, y= Photoperiod, color=Domain)) + geom_point(size=2) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")


##### combining mosquito count data with the neon data ####

### Lets first look at tmean and count data for only two domains 05 and 09


full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D09" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= Tmean7,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(./1),name="7 day mean temp (C)"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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


full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D09" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= PPT14/10,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(. *10),name="14 day total PPT (mm)"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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




full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D09" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= CumGDD/50,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(. *50),name="Cumulative GDD"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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


full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D05" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= Tmean7,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(./1),name="7 day mean temp (C)"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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


full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D05" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= PPT14/10,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(. *10),name="14 day total PPT (mm)"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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




full.df %>% filter(Year >2013 & Year < 2020) %>% 
  filter(Domain == "D05" & SciName == "Aedes vexans") %>% 
  ggplot() + 
  geom_point(aes(x=DOY, y= (Count/TrapHours),color=Plot ),size=2) +
  geom_line(aes(x=DOY, y= CumGDD/50,group=Plot),color="Red",size=1,alpha=.25) +
  theme_classic() + facet_wrap(~Year) + theme( legend.position = "None")+
  scale_y_continuous(sec.axis = sec_axis(~(. *50),name="Cumulative GDD"),
                     name = "A. vexans density")+ 
  xlab("DOY") +
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

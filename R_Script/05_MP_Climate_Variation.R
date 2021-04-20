####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/19/2021
# title: 05_MP_Climate_Variation

# The goal of the following script is to assess the variation in seonsality
# across the sites and domains of NEON an the climate deviation from
# 30 year average

# load/ install required libraries for prism data acquisition and clean up

#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )

# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# 30 year monthly averages
load("./Data/MonthlyNormals.Rda")

# Count data to get the domains and lat and long information
load("./Data/combinded.Rda")

# Subsetting to just plot information
complete.df <- complete.df %>%  ungroup %>% 
  select( c("Plot","Domain","Site", "Elev"))

# Just want the unique plot level info ( should only have 494 rows)
complete.df <- unique(complete.df)

# data structure for the daily data set

str(contigus.df)

#################### 1) Variation in seasonality across domains #############

## testing to see which domains have the highest variation in temperature and
## precipitation within a given season

season.df <- left_join(contigus.df, complete.df, by="Plot")

# Quantifying the seasonal SD for temperature and coefficient of variation for
# precipitation for all years except for 2013 as we only have a small sliver of 
# that year's data
season.df <- season.df %>% ungroup() %>% filter(Year >2013) %>% 
             group_by(Domain, Plot, Site, Elev, Year, Lat, Long) %>% 
             summarise(
               TempSD = sd(TMEAN, na.rm=T),
               PptCV = sd(PPT)/ mean(PPT)
             ) %>% ungroup()


## lets do some simple plotting to see how variation in temp and precip relate

season.df %>% # filter( Domain == "D10") %>% 
  ggplot(aes(x=TempSD, y=PptCV, color= Year)) + geom_point(size=3,alpha=.70) +
  facet_wrap(~Domain, scales="free") 

# How variation in temperature varies across domains
season.df %>% 
  ggplot(aes(x=reorder(Domain,-Lat), y=TempSD, fill= Domain)) + geom_boxplot(alpha=.5) 

# How seasonal patterns of precip varies across domains
season.df %>% 
  ggplot(aes(x=reorder(Domain,-Lat), y=PptCV, fill= Domain)) + geom_boxplot(alpha=.5) 

## Ok lets map this 
library(maps)
library(viridis)
MainStates <- map_data("state")

season.df %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptCV),alpha=.7,size=4) +
  scale_color_viridis(name = "Coef of Variation
(PPT)") + theme_classic()+ 
  xlab("Longitude") +ylab("Latitude")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )
  



season.df %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TempSD),alpha=.7,size=4) +
  scale_color_viridis(name = "SD (Temp)") + theme_classic()+ 
  xlab("Longitude") +ylab("Latitude")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



## Creating simple models to see which spatial factors ( Lat and elevation)
## predict seasonality (Higher values for SD and CV)

library(lme4)

season.temp.m <- lmer(TempSD ~ scale(Elev)+ scale(Lat) + (1|(Year))+ 
                   (1|Plot), data= season.df)

season.precip.m <- lmer(PptCV ~ scale(Elev)+
                          scale(Lat) + (1|(Year))+ 
                   (1|Plot), data= season.df)

summary(season.temp.m)  ## Higher seasonality as you go up in latitude
                        ## lower seasonality as you go up in elevation

summary(season.precip.m) # No effect of elevation on seasonality
                         # lower latitudes have higher seasonality of precip


#################### 2) Variation in seasonality across years #############


## Ok lets now check out to see which domains had the highest variation in 
## seasonal weather patterns across years and which locations had the smallest
## variation

annual.df <- left_join(contigus.df, complete.df, by="Plot")

annual.df$Month <- as.integer( lubridate::month(annual.df$Date) )

## Summarize data to the year and month level,
## mean for Tmin, Tmax and Tmeam
## sum for PPT 

annual.df <- annual.df %>% 
  group_by(Domain, Site, Elev, Lat, Long, Plot, Year, Month,) %>% 
  summarize(
    Tmean = mean(TMEAN, na.rm=T),
    Tmax = mean(TMAX, na.rm=T),
    Tmin = mean(TMIN, na.rm=T),
    PPT = sum(PPT, na.rm=T)
  ) %>% ungroup()

# Calculating the variation across years for each domain X month combo
annual.df <- annual.df %>% ungroup() %>% filter(Year >2013) %>% 
  group_by(Domain, Month, Plot, Site, Elev, Lat, Long) %>% 
  summarise(
    TempCV = sd(Tmean, na.rm=T),
    PptCV = sd(PPT)/ mean(PPT)
  ) %>% ungroup()

# Variation in temperature variation across domain
annual.df %>%  
  ggplot(aes(x=reorder(Domain,-TempCV),y=TempCV, fill=Domain)) + geom_boxplot()

# Variation in precip variation across domain
annual.df %>%  
  ggplot(aes(x=reorder(Domain,-PptCV),y=PptCV, fill=Domain)) + geom_boxplot()

## Mapping the variation in climate patterns across year
## across year variation in precipitation
annual.df  %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptCV),alpha=.7,size=4) +
  scale_color_viridis(name = "Coef of Variation
(PPT)") + theme_classic()+ 
  xlab("Longitude") +ylab("Latitude")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


## across year variation in temperature
annual.df  %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TempCV),alpha=.7,size=4)  +
  scale_color_viridis(name = "SD (Temp)") + theme_classic()+ 
  xlab("Longitude") +ylab("Latitude")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


## Variation in temperature across month , most of the variation is during the winter months
annual.df %>%  
  ggplot(aes(x=reorder(as.factor(Month),-TempCV),y=TempCV,
             fill=as.factor(Month))) + geom_boxplot()+ theme_classic()+ 
  xlab("Month") + ylab("Across year temp variation") +
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

# this effect is pretty consistent across all domains
annual.df %>%  
  ggplot(aes(x=reorder(as.factor(Month),-TempCV),y=TempCV, 
             fill=as.factor(Month))) + geom_boxplot()+
  facet_wrap(~Domain) + theme_classic()+ 
  xlab("Month") + ylab("Across year temp variation") +
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
         strip.text.x = element_text(size=14) )



## Variation in precip across month , pretty even variation across all months
annual.df %>%  
  ggplot(aes(x=reorder(as.factor(Month),-PptCV),y=PptCV,
             fill=as.factor(Month))) + geom_boxplot()+ theme_classic()+ 
  xlab("Month") + ylab("Across year Precip variation") +
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

# pretty variable across domain
annual.df %>%  
  ggplot(aes(x=reorder(as.factor(Month),-PptCV),y=PptCV,
             fill=as.factor(Month))) + geom_boxplot()+
  facet_wrap(~Domain) + theme(legend.position = "none")

## Simple model to assess which factors are associated with higher variation
season.m <- lmer(TempCV ~ scale(Elev) + scale(Lat)+
                   as.factor(Month)+ (1|(Domain)), data= annual.df)

summary(season.m) # higher  latitude there tends to be more variations

### Ok lets check to see which areas have the most deviation from the 30 year
### window
str(thirty.df)
thirty.df$Month <- as.integer(thirty.df$Month)

### adding month to daily data

contigus.df$Month <- as.integer( lubridate::month(contigus.df$Date) )

## Summarize data to the year and month level,
## mean for Tmin, Tmax and Tmeam
## sum for PPT 

DayMonth.df <- contigus.df %>% 
  group_by(Plot, Year, Month,) %>% 
  summarize(
    Tmean = mean(TMEAN, na.rm=T),
    Tmax = mean(TMAX, na.rm=T),
    Tmin = mean(TMIN, na.rm=T),
    PPT = sum(PPT, na.rm=T)
  ) %>% ungroup()

# Combined data sets


compare.df <- left_join( DayMonth.df, thirty.df, by= c("Plot",
                                                       "Month") )
compare.df <- left_join(compare.df, complete.df, by="Plot")

## Deviations from normal across domains 

#PPT A lot of variation,probably to be expected for a 4 year period
compare.df %>% 
  ggplot(aes(x=ppt_month_30y_ave, y=PPT, color=as.factor(Month)))+
  geom_point(size=2, alpha=.5)+geom_abline(intercept=0, slope=1)+
  facet_wrap(~Domain, scales="free")

# Temp, less variation, probably 
compare.df %>% 
  ggplot(aes(x=Tmax_month_30y_ave, y=Tmax, color=as.factor(Month)))+
  geom_point(size=2, alpha=.5)+geom_abline(intercept=0, slope=1)+
  facet_wrap(~Domain, scales="free")


## Calculating the deviations

compare.df$TmaxDev <- (compare.df$Tmax - compare.df$Tmax_month_30y_ave) 
compare.df$TminDev <- (compare.df$Tmin - compare.df$Tmin_month_30y_ave)
compare.df$TmeanDev <- (compare.df$Tmean - compare.df$Tmean_month_30y_ave)
compare.df$PptDev <- (log10(compare.df$PPT+1) - log10(compare.df$ppt_month_30y_ave+1))

## Variation in TMax
compare.df %>% 
  ggplot(aes(x=TmaxDev, fill=as.factor(Domain)))+
  geom_density( alpha=.5)+
  facet_wrap(~Month, scales="free")+ theme(legend.position = "None")

## Variation in PPT
compare.df %>% 
  ggplot(aes(x=PptDev, fill=as.factor(Domain)))+
  geom_density( alpha=.5)+
  facet_wrap(~Month, scales="free")+ theme(legend.position = "None")


## across year variation in temperature
compare.df  %>% filter(Year >2013) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptDev),alpha=.7,size=4) +
  scale_color_viridis()+facet_wrap(~Year)


compare.df  %>%  filter(Year >2013) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=4) +
  scale_color_viridis()+facet_wrap(~Year)

compare.df  %>%  filter(Year >2013  ) %>% filter(Month<5 | Month >10) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=4) +
  scale_color_viridis()+facet_wrap(~Year)

######################### Temperature ###########################

# Early season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 1|
                                                 Month == 2|
                                                 Month == 3) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=3) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "Temp 
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



# Mid-season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 4|
                                                   Month == 5|
                                                   Month == 6) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "Temp 
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



# Mid- late summer season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 7|
                                                   Month == 8|
                                                   Month == 9) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "Temp 
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


#Late season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 10|
                                                   Month ==11|
                                                   Month == 12) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=TmaxDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "Temp 
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

######################### Precipitation ###########################

# Early season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 1|
                                                   Month == 2|
                                                   Month == 3) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "PPT
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

# Midseason temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 4|
                                                   Month == 5|
                                                   Month == 6) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "PPT
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

# Mid- late summer season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 7|
                                                   Month == 8|
                                                   Month == 9) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "PPT
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


#Late season temp deviations
compare.df  %>%  filter(Year >2013  ) %>% filter(Month == 10|
                                                   Month ==11|
                                                   Month == 12) %>% 
  ggplot() +
  geom_polygon(data = MainStates, aes(x=long,y=lat, group=group),color="black",
               fill="white")+
  geom_point(aes(x=Long, y=Lat,  color=PptDev),alpha=.7,size=4) +
  facet_wrap(~Year)+
  scale_color_viridis(name = "PPT
Deviations") + theme_classic()+ 
  xlab("Longitude") + ylab("Latitude") +
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         #legend.position = c(.9,.2),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


## Relation between deviation in temp and deviation in ppt
compare.df %>% 
  ggplot(aes(x=TmaxDev, y=PptDev, color=as.factor(Domain)))+
  geom_point( alpha=.5)+ stat_smooth(method="lm", se=F)+
  facet_wrap(~Month, scales="free")+ theme(legend.position = "None")


compare.df %>% 
  ggplot(aes(x=TmaxDev, fill=as.factor(Year)))+
  geom_density( alpha=.5)+
  facet_wrap(~Domain, scales="free")

compare.df %>% 
  ggplot(aes(x=PptDev, fill=as.factor(Year)))+
  geom_density( alpha=.5)+
  facet_wrap(~Domain, scales="free")


compare.df %>% 
  ggplot(aes(x=TmaxDev, fill=as.factor(Domain)))+
  geom_density( alpha=.5)+
  facet_wrap(~Domain, scales="free")

compare.df %>% 
  ggplot(aes(x=PptDev, fill=as.factor(Domain)))+
  geom_density( alpha=.5)+
  facet_wrap(~Domain, scales="free")


### Exploring looking at PCA to better understand variation in the deviations
### across domains and months

## it is a bit convoluted so maybe come back later

compare.df$MonthAbb <- month.abb[compare.df$Month]

ppt.df <- compare.df %>% group_by(Domain,Plot, Year, Site) %>% filter(Year>2013) %>% 
  pivot_wider(names_from=MonthAbb, values_from= PptDev) %>% 
  summarize(
    Oct = max(Oct,na.rm=T),
    Nov = max(Nov,na.rm=T),
    Dec = max(Dec,na.rm=T),
    Jan = max(Jan,na.rm=T),
    Feb = max(Feb,na.rm=T),
    Mar = max(Mar,na.rm=T),
    Apr = max(Apr,na.rm=T),
    May = max(May,na.rm=T),
    Jun = max(Jun,na.rm=T),
    Jul = max(Jul,na.rm=T),
    Aug = max(Aug,na.rm=T),
    Sep = max(Sep,na.rm=T)
  ) %>% ungroup()

at1 <- FactoMineR::PCA(ppt.df[,5:16])

ppt.df$PCA1 <- at1$ind$coord[,1]
ppt.df$PCA2 <- at1$ind$coord[,2]


ggplot( ppt.df, aes(x =PCA1, y= PCA2, color=Domain)) + geom_point() +
  stat_ellipse() +facet_wrap(~Year)+
  theme_classic()+ geom_hline(yintercept = 0) + geom_vline(xintercept = 0)



tmean.df <- compare.df %>% group_by(Domain,Plot, Year, Site) %>% filter(Year>2013) %>% 
  pivot_wider(names_from=MonthAbb, values_from= TmeanDev) %>% 
  summarize(
    Oct = max(Oct,na.rm=T),
    Nov = max(Nov,na.rm=T),
    Dec = max(Dec,na.rm=T),
    Jan = max(Jan,na.rm=T),
    Feb = max(Feb,na.rm=T),
    Mar = max(Mar,na.rm=T),
    Apr = max(Apr,na.rm=T),
    May = max(May,na.rm=T),
    Jun = max(Jun,na.rm=T),
    Jul = max(Jul,na.rm=T),
    Aug = max(Aug,na.rm=T),
    Sep = max(Sep,na.rm=T)
  ) %>% ungroup()

at1 <- FactoMineR::PCA(tmean.df[,5:16])

tmean.df$PCA1 <- at1$ind$coord[,1]
tmean.df$PCA2 <- at1$ind$coord[,2]


ggplot( tmean.df, aes(x =PCA1, y= PCA2, color=Domain)) + geom_point() +
  stat_ellipse() +facet_wrap(~Year)+
  theme_classic()+ geom_hline(yintercept = 0) + geom_vline(xintercept = 0)




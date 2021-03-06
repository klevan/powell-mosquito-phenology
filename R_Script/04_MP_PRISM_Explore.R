####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/16/2021
# title: 04_MP_PRISM_Explore

# The goal of the following script is to explore the prism data to 

# load/ install required libraries for prism data acquisition and clean up

#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )

# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrism.Rda")

# 30 year monthly averages
load("./Data/MonthlyNormals.Rda")

### Data structure

str(daily.df) ## A LOT of rows (1,127,802!), 11 columns
              ## Daily weather data for all 494 plots from 
              ## start of water year 2014 (10/01/2013) to the end of 
              ## 2019 (12/31/2019)
              ## PPT daily precipitation in mm
              ## Tmean = daily mean temperature in C
              ## TMax = daily max temperature in C
              ## Tmin = daily min temperature in C
              ## Other columns
              ## Plot = Unique Plot Id used in mosquito sampling
              ## Lat = latitude of plot
              ## Long = longitude of plot
              ## Date = date of weather data
              ## Julian = running date number starting with 10/01/2013
              ## DOY = Day of year of the date
              ## Year = Year of data


str(thirty.df) ## 5928 obs, 8 columns
               ## 30 year monthly average climate data for all 494 plots
               ## Climate data
               ## ppt_month_30y_ave = monthly total ppt in mm
               ## Tmax_month_30y_ave = mean monthly daily mean max temp in C
               ## Tmin_month_30y_ave = mean monthly daily mean min temp in C
               ## Tmean_month_30y_ave = mean monthly daily mean temp in C
               ## Other columns
               ## Plot + unique plot id used in mosquito sampling
               ## Lat = Latitude of plot
               ## Long = longitude of plot
               ## Month = Month of interest


daily.df  %>% filter( Lat < 50 & Lat >25 ) %>%
  ggplot( aes(x=Julian, y = TMIN, color=Lat)) + geom_point() +
  scale_color_gradientn(colours = rainbow(5))


### Adding climate data that tracks weekly trends

## Adding a dummy variable to discern if a rain event occurred or not, using 
## 0.5 mm as my cutoff , can change later if we just want >0

daily.df$Rain[daily.df$PPT >=0.5]  <- 1
daily.df$Rain[daily.df$PPT < 0.5]  <- 0

#  7 Day moving average for temperature
daily.df$Tmean7 <- NA
daily.df$Tmax7 <- NA
daily.df$Tmin7 <- NA

# 14 day moving total for precipitation and raining days
daily.df$PPT14 <- NA
daily.df$Rain14 <- NA


## For loop to get the moving average 

for( p in 1:length(unique(daily.df$Plot))){
  FocPlot <- unique(daily.df$Plot)[p]
  plot.df <- filter(daily.df, Plot == FocPlot)
  plot.df <- arrange(plot.df, Julian)
  # 7 day moving average for temp
  for( t in 7:max(plot.df$Julian)){
    daily.df$Tmax7[daily.df$Plot==FocPlot & daily.df$Julian == t ] <- 
      mean(plot.df$TMAX[(t-6):t], na.rm=T)
    daily.df$Tmin7[daily.df$Plot==FocPlot & daily.df$Julian == t ] <- 
      mean(plot.df$TMIN[(t-6):t], na.rm=T)
    daily.df$Tmean7[daily.df$Plot==FocPlot & daily.df$Julian == t ] <- 
      mean(plot.df$TMEAN[(t-6):t],na.rm=T)
    # 14 day moving average for precip
    if( t >= 14){
      daily.df$PPT14[daily.df$Plot==FocPlot & daily.df$Julian == t ] <- 
        sum(plot.df$PPT[(t-13):t], na.rm=T)
      daily.df$Rain14[daily.df$Plot==FocPlot & daily.df$Julian == t ] <- 
        sum(plot.df$Rain[(t-13):t], na.rm=T)
    }
  }
}

# save(daily.df, file= "DailyPrismMod.Rda")


daily.df %>% filter(Plot == "ABBY_037" & Year > 2013) %>%
  ggplot(aes(x = DOY, y=Rain14, 
             color= as.factor(Year) ))+geom_point()+
  facet_wrap(~Year)

## should have done this earlier but lets remove NAs in the PRISM data 
## this should remove NEON plots that are outside the contiguous USA

contigus.df <- daily.df[!is.na(daily.df$TMAX),]

## Adding photoperiod
library(meteor)

contigus.df$Photoperiod <- photoperiod(contigus.df$DOY, contigus.df$Lat)

## Adding growing degree days
## simply the mean daily temp - base temp, base temp == 10 C

contigus.df$GDD <- contigus.df$TMEAN - 10

# if GDD < 0 then setting GDD to 0
contigus.df$GDD[contigus.df$GDD<0] <- 0

# Setting temperature max at 33 degrees , if mean was above 33c GDD == 0
contigus.df$GDD[contigus.df$TMEAN>33] <- 0


# Adding cumulative GDD for each plot by year combo

contigus.df <- contigus.df %>% group_by(Plot, Year) %>% arrange(Julian) %>% 
  mutate( CumGDD = cumsum( GDD )) %>% ungroup()

contigus.df %>% filter(  Plot == "ABBY_037" |
                 Plot == "DSNY_075" |
                 Plot == "MOAB_036" |
                 Plot == "CPER_086" |
                 Plot == "WOOD_034" |
                 Plot == "SOAP_041" ) %>% 
  ggplot(aes(x=DOY, y= CumGDD, color=as.factor(Year))) + 
  geom_point(size=2, alpha=.5)+
  facet_wrap(~Plot, scales="free_y")

#save(contigus.df, file= "DailyPrismMod.Rda")

####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/14/2021
# title: 03_MP_PRISM_Data

# The goal of the following script is to download daily weather data from prism
# to eventually link to the mosquito trapping data

# load/ install required libraries for data acquisition and clean up

install.packages("devtools")
library(devtools)
install_github("ropensci/prism")
library(prism)
library( dplyr)
library( tidyr )
library( ggplot2 )

## Set download folder for 30 year normal
prism_set_dl_dir("C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals")

# downloading 30 year monthly normals

# Temperature #

# mean value 
get_prism_normals(type ='tmean', resolution = '4km', mon=1:12, keepZip = F)

# max value 
get_prism_normals(type ='tmax', resolution = '4km', mon=1:12, keepZip = F)

# min value
get_prism_normals(type ='tmin', resolution = '4km', mon=1:12, keepZip = F)

# Precipitation  #

get_prism_normals(type ='ppt', resolution = '4km', mon=1:12, keepZip = F)

# Set download folder for daily data

prism_set_dl_dir("C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys")

## temperature ##

## mean temp
get_prism_dailys(
  type = "tmean", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)

## max temp
get_prism_dailys(
  type = "tmax", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)

## min temp
get_prism_dailys(
  type = "tmin", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)

## Precipitation
get_prism_dailys(
  type = "ppt", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)

##### Woot downloaded all the daily and normal data, now lets subset it down
#### to the sites of interest
# input combined dataset created in "01_MP_Data_Clean_Up.R"
#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

load("./Data/combinded.Rda")

complete.df <- ungroup(complete.df)
plot.df <- dplyr::select(complete.df, c("Plot", "Lat", "Long")) 

plot.df <- unique(plot.df)
plot.spdf <- SpatialPointsDataFrame( coords= plot.df[,c("Long", "Lat")], 
                                     data = plot.df, 
                                     proj4string = CRS("+proj=longlat  +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs"))

### Monthly ppt

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/PPT")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,ppt_month_30y_ave, 4:ncol(MP.Plots))

Mp.Plots$Month <- gsub("PRISM_ppt_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

MonthPPT.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                           "ppt_month_30y_ave"))

### Monthly tmean

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmean")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmean_month_30y_ave, 4:ncol(MP.Plots))

Mp.Plots$Month <- gsub("PRISM_tmean_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

MonthTmean.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                            "Tmean_month_30y_ave"))


### Monthly tmax

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmax")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmax_month_30y_ave, 4:ncol(MP.Plots))

Mp.Plots$Month <- gsub("PRISM_tmax_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

MonthTmax.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                              "Tmax_month_30y_ave"))



### Monthly tmin

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmin")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmin_month_30y_ave, 4:ncol(MP.Plots))

Mp.Plots$Month <- gsub("PRISM_tmin_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

MonthTmin.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                             "Tmin_month_30y_ave"))

## Combining all of the monthly data
thirty.df <- left_join(MonthPPT.df, MonthTmax.df, by=c("Plot", "Lat", "Long",
                                                       "Month"))

thirty.df <- left_join(thirty.df, MonthTmean.df, by=c("Plot", "Lat", "Long",
                                                       "Month"))

thirty.df <- left_join(thirty.df, MonthTmin.df, by=c("Plot", "Lat", "Long",
                                                      "Month"))

ggplot(thirty.df, aes(x=Lat, y=Tmax_month_30y_ave, color=Month))+
  geom_point()+ xlim(27,47)


ggplot(thirty.df, aes(x=Lat, y=ppt_month_30y_ave, color=Month))+
  geom_point()+ xlim(27,47)



### Ok lets do daily data
### Daily ppt

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/PPT")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,PTT, 4:ncol(MP.Plots))

Mp.Plots$Date <- gsub("PRISM_ptt_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

## getting the dates right
## extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )


Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday


DailyPPT.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                              "Year","Julian", "DOY",
                                              "PPT"))


### Daily Mean

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmean")

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMEAN, 4:ncol(MP.Plots))

Mp.Plots$Date <- gsub("PRISM_tmean_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

## getting the dates right
## extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
            separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
            dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )


Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday


DailyTMEAN.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                            "Year","Julian", "DOY",
                                             "TMEAN"))

### Daily Max

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmax")

get_prism_dailys(
  type = "tmax", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)


prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMAX, 4:ncol(MP.Plots))

Mp.Plots$Date <- gsub("PRISM_tmax_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

## getting the dates right
## extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )


Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday


DailyTMAX.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                              "Year","Julian", "DOY",
                                              "TMAX"))



### Daily Min

options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmin")

get_prism_dailys(
  type = "tmin", 
  minDate = "2013-10-01", 
  maxDate = "2019-12-31", 
  keepZip = FALSE
)


prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls())

proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## lets extract the points of interest

MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMIN, 4:ncol(MP.Plots))

Mp.Plots$Date <- gsub("PRISM_tmin_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

## getting the dates right
## extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )


Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday


DailyTMIN.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                             "Year","Julian", "DOY",
                                             "TMIN"))


## Combining all of the daily
daily.df <- left_join(DailyPPT.df, DailyTMAX.df, by=c("Plot", "Lat", "Long",
                                                       "Date","Year", "Julian",
                                                       "DOY") )
daily.df <- left_join(daily.df, DailyTMIN.df, by=c("Plot", "Lat", "Long",
                                                      "Date","Year", "Julian",
                                                      "DOY") )

daily.df <- left_join(daily.df, DailyTMEAN.df, by=c("Plot", "Lat", "Long",
                                                      "Date","Year", "Julian",
                                                      "DOY") )


## SAving the 30 year normal
save(thirty.df, file = "MonthlyNormals.Rda")


##  Saving the daily data
save(daily.df, file= "DailyPrism.Rda")
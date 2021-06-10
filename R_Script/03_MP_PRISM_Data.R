####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/14/2021
# title: 03_MP_PRISM_Data

# The goal of the following script is to download 30 year monthly average climate
# data and daily weather data from prism to eventually link to the
# mosquito trapping data

# load/ install required libraries for prism data acquisition and clean up

library( prism )
library( dplyr )
library( tidyr )
library( ggplot2 )

# input combined data set created in "01_MP_Data_Clean_Up.R" to
# extract lat and long from all the plots

#Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

load("./Data/Mosquito_Data_Clean.Rda")

complete.df <- ungroup(complete.df) # ungrouping so i can extract col. of interest

plot.df <- dplyr::select(complete.df, c("Plot", "Lat", "Long")) # selecting col.

plot.df <- unique(plot.df) # removing duplicate rows , show be # 494 X 3

## Making it a SP dataframe
plot.spdf <- SpatialPointsDataFrame( coords= plot.df[,c("Long", "Lat")], 
                                     data = plot.df, 
                                     proj4string = CRS("+proj=longlat  +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs"))

### 30 year monthly average for total ppt

# Setting a path to store the prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/PPT")

# Downloading the data 
#get_prism_normals(type ='ppt', resolution = '4km', mon=1:12, keepZip = F)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

RS <- pd_stack(prism_archive_ls()) # Stacking the downloaded data

# Setting the project
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

# Extracting plots level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Make it into a data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,ppt_month_30y_ave, 4:ncol(MP.Plots))

# Extracting month from name
Mp.Plots$Month <- gsub("PRISM_ppt_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

# Select columns of interest to store and merge later
MonthPPT.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                           "ppt_month_30y_ave"))

### Monthly tmean

# Setting pathway to store prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmean")

#Downloading data
#get_prism_normals(type ='tmean', resolution = '4km', mon=1:12, keepZip = F)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stacking prism data
RS <- pd_stack(prism_archive_ls())

# Setting the projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## extracting plot level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Convert to dataframe
Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmean_month_30y_ave, 4:ncol(MP.Plots))

# Extracting month
Mp.Plots$Month <- gsub("PRISM_tmean_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

# Select columns of interest to store and merge later
MonthTmean.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                            "Tmean_month_30y_ave"))


### Monthly tmax
# Setting path to store prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmax")

# downloading prism data
#get_prism_normals(type ='tmax', resolution = '4km', mon=1:12, keepZip = F)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stacking PRISM data
RS <- pd_stack(prism_archive_ls())

# Setting the projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## extracting prism points
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# convert to dataframe
Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmax_month_30y_ave, 4:ncol(MP.Plots))

# Extract month
Mp.Plots$Month <- gsub("PRISM_tmax_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

# Select columns of interest to store and merge later
MonthTmax.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                              "Tmax_month_30y_ave"))

### Monthly tmin

# Path to store prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Monthly_Normals/Tmin")

# downloading prism data
#get_prism_normals(type ='tmin', resolution = '4km', mon=1:12, keepZip = F)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

#Stack Prism data
RS <- pd_stack(prism_archive_ls())

# Setting projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## Extracting plot level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# COnvert to data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Month,Tmin_month_30y_ave, 4:ncol(MP.Plots))

# Extract Month
Mp.Plots$Month <- gsub("PRISM_tmin_30yr_normal_4kmM2_", "", Mp.Plots$Month) %>%
  gsub("_bil", "", .)

# Select columns of interest to store and merge later
MonthTmin.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Month",
                                             "Tmin_month_30y_ave"))


## Merging all of the 30 year monthly averages ##

thirty.df <- left_join(MonthPPT.df, MonthTmax.df, by=c("Plot", "Lat", "Long",
                                                       "Month"))

thirty.df <- left_join(thirty.df, MonthTmean.df, by=c("Plot", "Lat", "Long",
                                                       "Month"))

thirty.df <- left_join(thirty.df, MonthTmin.df, by=c("Plot", "Lat", "Long",
                                                      "Month"))

### PRISM DAILY DATA ###

# These files are going to be big

### Daily ppt

# Set path for daily ppt
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/PPT")

# Download daily data
#get_prism_dailys(
#  type = "ppt", 
#  minDate = "2013-10-01", 
#  maxDate = "2019-12-31", 
#  keepZip = FALSE
#)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stack daily data
RS <- pd_stack(prism_archive_ls())

# Set projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

# Extracting plot level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Convert to data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,PTT, 4:ncol(MP.Plots))

# Extracting date
Mp.Plots$Date <- gsub("PRISM_ptt_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

## extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

# extracing Julian data
Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )

# extracting DOY
Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday

# Select columns for merging later
DailyPPT.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                              "Year","Julian", "DOY",
                                              "PPT"))


### Daily Mean

# path to store downloaded data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmean")

# Dowload daily data
#get_prism_dailys(
#  type = "tmean", 
#  minDate = "2013-10-01", 
#  maxDate = "2019-12-31", 
#  keepZip = FALSE
#)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stack daily data
RS <- pd_stack(prism_archive_ls())

# Setting projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

## Extracting points of interest
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Converting to a data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMEAN, 4:ncol(MP.Plots))

# Extracting date
Mp.Plots$Date <- gsub("PRISM_tmean_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

# Extracting year
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
            separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
            dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

# Converting to julian date
Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )

# Converting to DOY
Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday

# Selecting columns to merge later
DailyTMEAN.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                            "Year","Julian", "DOY",
                                             "TMEAN"))

### Daily Max

# Setting a path to store prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmax")

#get_prism_dailys(
#  type = "tmax", 
#  minDate = "2013-10-01", 
#  maxDate = "2019-12-31", 
#  keepZip = FALSE
#)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stack data
RS <- pd_stack(prism_archive_ls())

# Setting projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

# Extracting plot level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Converting to data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMAX, 4:ncol(MP.Plots))

# Extracting date
Mp.Plots$Date <- gsub("PRISM_tmax_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

# Extracting year from date
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

# Extracting Julian date
Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )

# Extracting DOY
Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday

# Selecting columns to merge later
DailyTMAX.df <- Mp.Plots %>% dplyr::select(c("Plot", "Lat", "Long", "Date",
                                              "Year","Julian", "DOY",
                                              "TMAX"))

### Daily Min

# Setting path to store prism data
options(prism.path ="C:/Users/tmcdevitt-galles/powell-mosquito-phenology/Data/PRISM/Dailys/Tmin")

# Downloading data
#get_prism_dailys(
#  type = "tmin", 
#  minDate = "2013-10-01", 
#  maxDate = "2019-12-31", 
#  keepZip = FALSE
#)

prism_archive_ls() ## make sure i am looking at 30 year normal ppt

# Stacking data
RS <- pd_stack(prism_archive_ls())

# Setting Projection
proj4string(RS) <- CRS( "+proj=longlat +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")

# Extracting plot level data
MP.Plots <- raster::extract(RS, plot.spdf,method = "bilinear", sp=T)

# Convert to data frame
Mp.Plots <- data.frame(MP.Plots) %>% gather(Date,TMIN, 4:ncol(MP.Plots))

# Extracting date
Mp.Plots$Date <- gsub("PRISM_tmin_stable_4kmD2_", "", Mp.Plots$Date) %>%
  gsub("_bil", "", .)

# extracting year from date
Mp.Plots$dumDate <- as.integer(Mp.Plots$Date)

Mp.Plots <- Mp.Plots %>% 
  separate( dumDate, c("Year", "Throw1"), sep =4 ) %>%
  dplyr::select(-"Throw1")

# Converting to proper date format
Mp.Plots$Date <- as.Date(as.factor(Mp.Plots$Date), "%Y%m%d")

# COnverting t Julian date
Mp.Plots$Julian <- julian(Mp.Plots$Date,
                          origin = as.Date("2013-10-01") )

# extracting DOY
Mp.Plots$DOY <- as.POSIXlt(Mp.Plots$Date,
                           format="%Y-%m-%d")$yday

# Selecting columns for merging
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

## Save all the data as Rdata so we dont have to do this costly process again

## SAving the 30 year normal
save(thirty.df, file = "MonthlyNormals.Rda")


##  Saving the daily data
save(daily.df, file= "DailyPrism.Rda")
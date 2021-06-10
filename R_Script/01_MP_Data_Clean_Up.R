####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 03/31/2021
# title: 01_MP_Data_Clean_U8p

# The goal of the following script is to import and clean the NEON mosquito data
# as well as create some summary stats to better understand the data

# load required libraries for data clean up

library( dplyr )
library( tidyr )
library( ggplot2 )

## Set working directory

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

# input raw data

# Mosquito trapping data
trap.df <- read.csv( "./Data/Mos_Data/Mos_Trap.csv" )

# Mosquito count data
count.df <- read.csv( "./Data/Mos_Data/Mos_Count.csv" )

# Mosquito sorting data

sort.df <- read.csv( "./Data/Mos_Data/Mos_Sort.csv" )

#### Initial data assessment

dim( trap.df ) # 54691 X 32

str(trap.df)

dim( count.df ) # 105374 X 43

str( count.df )

dim( sort.df ) # 29784 X 21

str( sort.df )

# trap.df = trapping information including domain, site ,plot, plot information,
# lat and long, elevation, trap date, collection date, total time and
# additional information regarding the quality of the traps , fan working ect
#
# sort.df = information regarding the sorting of the mosquitoes, important for
# scaling count data up for when the count was only on a proportion of the coll.
# 
# count.df  = individual count data from trapping nets, includes counts for all
# taxa identified in traps, seems like most taxa goes to species and has other
# taxonomic leves ( IE Gensus, Tribe, Family, ect)

## Simplif
trap.df <- dplyr::select(trap.df, c("domainID", "siteID","plotID",
                                    "plotType", "nlcdClass","decimalLatitude",
                                    "decimalLongitude","elevation", "setDate",
                                    "collectDate", "trapHours", "nightOrDay",
                                    "sampleID", "sampleTiming", "fanStatus",
                                    "dryIceStatus", "sampleCondition"))

colnames(trap.df) <- c("Domain", "Site","Plot",
                       "plotType", "VegClass","Lat",
                       "Long","Elev", "SetDate",
                       "CollectDate", "TrapHours", "NorD",
                       "SampleID", "SampleTiming", "FanStatus","IceStatus",
                       "SampleCondition")



sort.df <- dplyr::select(sort.df, c("plotID",
                                     "setDate", "totalWeight",
                                    "subsampleWeight"))

colnames(sort.df) <- c("Plot", "SetDate", "TotalWeight", "SubsetWeight")


trap.df <- left_join(trap.df, sort.df, by=c("Plot", "SetDate"))



## adding date, year, julian and DOY metrics

trap.df$dum.date <- trap.df$CollectDate

trap.df <-  trap.df%>%
  separate( dum.date , c("dum.date", "Throw1"), sep =10) %>% 
  dplyr::select( -("Throw1"))

trap.df$Date <- as.Date( trap.df$dum.date,"%Y-%m-%d")

trap.df <-  trap.df%>%
  separate(dum.date , c("Year", "Throw1"), sep =4) %>%
  dplyr::select( -"Throw1")



trap.df$Julian <-  julian( trap.df$Date , 
                           origin = as.Date("2014-01-01"))

trap.df$DOY <- as.POSIXlt( trap.df$Date, 
                           format= "%Y-%m-%d")$yday

# Checking out some variation in traping data

# Different types of sample conditons
unique(trap.df$SampleCondition)
# Talk to Sara and Katie about the different types of sample conitions
#   Cold chain broke, other, sample incomplete, "" , 
#   handling error and damanged

# Number of different types of sample condition
trap.df %>% group_by(SampleCondition) %>% 
  summarize( nObs = n(),
             per = n()/nrow(trap.df))

## 98% of all events had no compromise in the sample

## Sampling events with fan off

trap.df %>% group_by(FanStatus) %>% 
  summarize( nObs = n(),
             per = n()/nrow(trap.df))
# 98% of the sampling events had the fan on

# Ice status 
trap.df %>% group_by(IceStatus) %>% 
  summarize( nObs = n(),
             per = n()/nrow(trap.df))
# 98% of the samplling events had ice

# Field status 
trap.df %>% group_by(SampleTiming) %>% 
  summarize( nObs = n(),
             per = n()/nrow(trap.df))
# 98% of the samplling events were doing the field season


### looking at the distribution of sampling events

trap.df %>% filter(Year !="2020" ) %>% 
  ggplot(aes(x=DOY, fill=Year)) + geom_density(alpha=.5) +
  facet_wrap(~Domain, scales="free_y") + theme_classic()

year.df <- trap.df %>% group_by(Year, Domain) %>% 
  summarise(start = min(DOY, na.rm = T),
            end = max(DOY), na.rm = T,
            nDates = length(unique(DOY)))

# remove 2020 and wanting to find at least 10 sampling event in a year
# we can relax of contrain these requirements later after some discussion
year.df %>% filter(nDates > 10 & Year != "2020") %>% 
  group_by(Domain) %>% 
  summarize(
    mStart = mean(start),
    mEnd = mean(end),
    nDays = mean(nDates),
    nYears = n()
  )

## D01 = northeast , mean start date = EarlyApril, mean end date = End Nov
##      40 sampling events per year, 5 years with at least 10 sampling events
##       
## D02 = Mid-atlantic, mean start date = Mid Feb  mean end date = Mid Dec
##       61 unique sampling events per year, 6 years with at least 10 sampling 
##       
## D03 = Southeast, mean start date = Mid Feb mean end date = Early Dec
##       65 unique sampling eventsd per year, 5 years with at least 10 sampling
##       
## D04 = Neotropical NOT PART OF THE PROJECT
## 
## D05 = Great lakes, mean start date = Early April, mean end date = mid Nov
##       49 unique sampling events , 5 years of data
##       
## D06 = Praire, mean start date = Late Jan, mean end date = Mid Nov
##       41 unique sampling dates, 4 years of data
##       
## D07 = Appalachian, mean start date = Late Feb, mean end date Mid DeC
##       52 unique sampling dates, 5 years of data
##       
## D08 = Ozarks, mean start date = Mid Feb, mean end date = Early December
##       58 sampling events , 5 years of data
##       
## D09 = Northern plains, mean start date = End April, mean end date = Early Nov
##       40 unqieu sampling eventsm 5 years of data
##       
## D10 = Central Plains, mean start date= Late Feb, mean end date = Mid Nov
##       49 unique sampling events, 5 years of data
##       
## D11 = Southern Plains, mean start date = Late Jan, mean end date = End Dec
##       58 unique sampling events, 4 years of data
##       
## D12 = Northern Rockies, mean start date = Mid May, mean end date = Mid Nov
##       21 unique sampling events, 2 year of data
##       
## D13 = Southern Rockies, mean start date = Mid April, mean end date = Late Oct
##       26 unique sampling events, 4 years of data
##       
## D14 = Desert SW, mean start date =  Mid Feb, Mean end date = Late Dec
##       58 unique sampling events, 4 years of data
##       
## D15 = Great Basin, mean start date = Mid March, mean end date = Mid Nov
##       35 unique sampling evengs, 5 years of data
##       
## D16 = Pacific NW, mean start date = End April, mean end date = Mid Nov
##       19 unique sampling events, 3 years of data
##       
## D17 = Pacific SW, mean start date = Early Jan, mean end date = End Dec
##       54 unqiue sampling events, 3 years of data
##       
## D18 = Tundra NOT PART OF THE PROJECT
## 
## D19 = Tiaga, NOT PART OF THE PROJECT 
## 

# Exploring the patterns at the plot levellevel

plot.df <- trap.df%>% 
  group_by(Year, Domain, Site, Plot, NorD) %>% 
  summarise(start = min(DOY, na.rm = T),
            end = max(DOY, na.rm = T),
            nDates = length(unique(DOY))) %>% ungroup() 
# removing missing year and 2020 
plot.df <- (plot.df %>% filter( Year !=  "2020" & 
                                  Year != "" ) )
# calculating the length of the sampling season 
plot.df$Season <- plot.df$end - plot.df$start

## the relationship between  to season length and number of sampling events
ggplot(plot.df, aes(x = Season, y= nDates, color=Site)) +geom_point(size=2)+
  theme(legend.position="none") + geom_hline(yintercept = 10, size =1) +
  facet_wrap(~Domain, scales="free") 

## calculating the mean difference between sampling dates
plot.df$SGap <- plot.df$Season/plot.df$nDates

ggplot(plot.df, aes(x = Season, y= SGap, color=Domain, size=nDates)) + 
  geom_point(alpha=.5)+
  theme(legend.position="none") + facet_wrap(~Year) + geom_hline(yintercept=14)


### lets make sure these gaps are correct


plot.df <- trap.df%>% 
  group_by(Year, Domain, Site, Plot, NorD) %>% 
  mutate(start = min(DOY, na.rm = T),
            end = max(DOY, na.rm = T),
            nDates = length(unique(DOY))) %>% ungroup() 

########## Count data ###########

## Simplifying count data

count.df <- dplyr::select(count.df, c("domainID", "siteID","plotID",
                                      "setDate", "collectDate", "subsampleID",
                                      "individualCount","sex","family", "genus", 
                                      "specificEpithet", "scientificName"))

colnames(count.df) <- c("Domain", "Site","Plot",
                        "SetDate", "CollectDate", "subsampleID",
                        "Count","Sex", "Family", "Genus",
                        "Species", "SciName")



## splitting subsample ID to use for merging with the trap info

count.df$dumsampleID <- count.df$subsampleID

count.df <- count.df %>%
  separate( dumsampleID , c("sampleID", "Throw1"), sep =-5) %>% 
  dplyr::select( -"Throw1")

# Making new sample id a factor
count.df$SampleID <- as.factor(count.df$sampleID)



## Checks for consistency in species names
(unique(count.df$SciName))
## Couple of variation in taxonomic level that might require we merge things

## lets see what Aedes candaensis and its subspecies looks like

canada.df <- filter( count.df, SciName == 'Aedes canadensis canadensis' |
                               SciName == 'Aedes canadensis mathesoni'|
                               SciName == 'Aedes canadensis')


ggplot(canada.df, aes(x=log10(Count), fill =SciName )) + geom_density()+ facet_wrap(~Domain)


count.df$SciName[count.df$SciName == "Aedes canadensis mathesoni"] <-'Aedes canadensis'
count.df$SciName[count.df$SciName == "Aedes canadensis canadensis"] <-'Aedes canadensis'


# Combinding the two data sets
# Simplifying count data

count.df <- dplyr::select(count.df, c( "Plot", "SetDate",
                                       "Count","Sex","Family", "Genus",
                                       "Species", "SciName") )

full.df <- left_join(trap.df, count.df, by=c("Plot", "SetDate"))
dim(full.df) # 131676 x 27

## assessing what rows cant join, hmm looks like these dont have sampleID!

## Are these site and years where these is no mosquitos collected?
anti.df <- anti_join(trap.df, count.df,by=c("Plot", "SetDate"))
dim(anti.df) # 26065 X 21


anti.df %>% 
  ggplot(aes(x=DOY, fill=Year)) + geom_density(alpha=.5)+
  facet_wrap(~Domain, scales="free_y")


str(full.df)


##  ok next major step is adding in 0s, i imagine this will be fairly important
##  for early/late dates where no mosquites would exist
##  
##  For step is just to see how stable species reporting is across time starting
##  at the domain, then site, then plot

## Im intersted in just seeing how stable the reporting is for species even if
## it is a "0"


spp.df <- full.df %>% filter(Count >0) %>% 
  group_by(Domain, Site, Plot,DOY, Year) %>% 
  summarize(
    nSpecies = length(unique(SciName))
  )



spp.df %>%  filter( Year != "2020" & Year != "") %>% 
  ggplot(aes(x=DOY, y= nSpecies, color=Year)) + geom_point(alpha=.75)+
  facet_wrap(~Domain) +theme_classic()


##### Lets add the zeros for taxa that were detected at least once in a 
##### plot by year 
##### ***********  Important note, for now i do not care about distinguishig
  
## Creating a unique sample id for all sampling vents
full.df$SampID <- paste( full.df$Plot,"-", full.df$Year, "-",full.df$DOY, "-",full.df$NorD,
             "-", full.df$TrapHours)
  
t <- 0
for( y in 1:length( unique(full.df$Year))){
  focYear <- unique(full.df$Year)[y] 
  select.df <- filter(full.df, Year== focYear)
  for( s in 1:length(unique(select.df$Plot))){
    focPlot <- unique(select.df$Plot)[s] 
    iso.df <- filter( select.df, Plot == focPlot)
    Species <- c(unique(iso.df$SciName))
    Event <- c(unique(iso.df$SampID))
    new.df <- expand.grid(Species, Event)
    colnames(new.df) <- c("SciName","SampID")
    new.df$Plot <- focPlot
    new.df$Year <- focYear
    if( t == 0){
      long.df <- new.df
      t <- 1
    }else{
      long.df <- rbind.data.frame(long.df, new.df)
    
    }
  }
}


simpleCount.df <- full.df %>% 
  group_by(SciName,SampID) %>% 
  summarise(Count=sum(Count,na.rm=T))


Zero.df <- right_join( simpleCount.df, long.df, 
                     by =c("SciName","SampID" ))


join.df <- full.df %>% dplyr::select(c("Domain", "Site", "Plot", "plotType", "VegClass",
                                "Lat","Long", "Elev" ,"NorD","TrapHours", 
                                "SampID", "Date", "DOY", "SampleCondition",
                                "TotalWeight", "SubsetWeight") )


complete.df <- left_join(Zero.df, join.df, by=c("Plot" ,"SampID"))


# all NAs in count data should be 0
complete.df$Count[is.na(complete.df$Count)] <- 0

## only interest in taxa we have down to species
complete.df <- complete.df[!is.na(complete.df$SciName),]
complete.df <- complete.df %>% filter(SciName != "")

# deleteing duplicate rows
complete.df <- unique(complete.df)

#



## if this worked there should be no trend across time 


spp.df <- complete.df %>% 
  group_by(Plot,DOY, Year) %>% 
  summarize(
    nSpecies = length(unique(SciName))
  )



spp.df %>%  filter( Year != "2020" & Year != "") %>% 
  ggplot(aes(x=DOY, y= nSpecies, color=Year)) + geom_point(alpha=.75)+
  facet_wrap(~Year,scales='free_y') +theme_classic()

# correcting the count based on subsample weight 

## first thing i need to do is add in 1 for all 0s

complete.df$SubsetWeight[is.na(complete.df$SubsetWeight)] <- 1
complete.df$SubsetWeight[complete.df$SubsetWeight == 0] <- complete.df$TotalWeight[complete.df$SubsetWeight == 0]
complete.df$TotalWeight[is.na(complete.df$TotalWeight)] <- 1

complete.df$Count_adj <- (complete.df$Count * complete.df$TotalWeight) / complete.df$SubsetWeight

# lets check to make sure this is correct, graph should be 1:1

ggplot(complete.df, aes(x=(SubsetWeight/TotalWeight), y = (Count/Count_adj)))+
  geom_point() + geom_abline(slope=1,intercept=0)

hist(log10(complete.df$Count_adj+1))

hist(log10(complete.df$Count+1))


ggplot(complete.df, aes(x=log10(Count+1), y = log10(Count_adj+1)))+
  geom_point() + geom_abline(slope=1,intercept=0)

# Should be good to start to explore

save(complete.df, file = "Mosquito_Data_Clean.Rda")


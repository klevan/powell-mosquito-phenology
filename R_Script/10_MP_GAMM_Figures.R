####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 10_MP_GAMM_Figures

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory
#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )a
library( ggplot2 )
library(gamm4)
# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# Count data to get the domains and lat and long information
load("./Data/combinded.Rda")

# data structure for the daily data set

str(contigus.df)

str(complete.df)

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

### PPT
gam.ppt <- gamm4( PPT14 ~ s(DOY,by=(fYear), bs="cc"),
                   random = ~ (1|Plot),
                   data=focal.df, family="gaussian")

## lets try and plot this with new data

DOY <- 0:365
fYear <- c(levels(focal.df$fYear))
plot.m <- c(levels(as.factor(focal.df$Plot)))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)


dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),plot.m)

dum.df$Pred <- predict(gam.ppt$gam, newdata = dum.df)



dum.df %>% filter( fYear != "2013") %>% 
  ggplot(  aes(x=DOY, y=(Pred),
               color=fYear))+ 
  #geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75)+ theme_classic()


ppt.df <- dum.df

colnames(ppt.df) <- c("DOY", "fYear", "Plot", "Pred")

ppt.df$Metric <- "PPT"

## photoperiod
gam.photo <- gamm4( Photoperiod ~ s(DOY,by=(fYear), bs="cc"),
                   random = ~ (1|Plot),
                   data=focal.df, family="gaussian")

## lets try and plot this with new data

DOY <- 0:365
fYear <- c(levels(focal.df$fYear))
plot.m <- c(levels(as.factor(focal.df$Plot)))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)


dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),plot.m)

dum.df$Pred <- predict(gam.photo$gam, newdata = dum.df)



dum.df %>% filter( fYear != "2013") %>% 
  ggplot(  aes(x=DOY, y=(Pred),
               color=fYear))+ 
  #geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75)+ theme_classic()


photo.df <- dum.df

colnames(photo.df) <- c("DOY", "fYear", "Plot", "Pred")

photo.df$Metric <- "Photo"



## photoperiod
gam.gdd <- gamm4( CumGDD ~ s(DOY,by=(fYear), bs="cc"),
                    random = ~ (1|Plot),
                    data=focal.df, family="gaussian")

## lets try and plot this with new data

DOY <- 0:365
fYear <- c(levels(focal.df$fYear))
plot.m <- c(levels(as.factor(focal.df$Plot)))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)


dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),plot.m)

dum.df$Pred <- predict(gam.gdd$gam, newdata = dum.df)



dum.df %>% filter( fYear != "2013") %>% 
  ggplot(  aes(x=DOY, y=(Pred),
               color=fYear))+ 
  #geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75)+ theme_classic()


gdd.df <- dum.df

colnames(gdd.df) <- c("DOY", "fYear", "Plot", "Pred")

gdd.df$Metric <- "GDD"

abio.df <- rbind.data.frame(temp.df,photo.df,ppt.df, gdd.df)

abio.df <- filter(abio.df, fYear != "2013")


ggplot( abio.df, aes(x=DOY, y= Pred, color=fYear))+
  geom_line(size=2, alpha=.75) + facet_wrap(~Metric)

#################### Combining data sets #############

# selecting the needed columns and data to merge with count data
cont.df <- contigus.df %>% dplyr::select( -c("Lat", "Long", "Date"))

# Joining data sets so that prism data is now linked to count data
full.df <- left_join(complete.df, cont.df, by=c("Plot","Year","DOY"))

## Number of rows should match complete.df of 620662
dim(full.df) # 620662 X 30



ggplot(full.df, aes(x= CumGDD, y= log10( (Count/TrapHours)+1 )))+ geom_point()
############ subsetting to the single species, site and year #########

# We are only interested in Aedes vexans from WOOD site
toy.df <- filter(full.df, SciName== "Aedes vexans" & Domain == "D09")

unique(toy.df$Year)

## summarize to DOY level
toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot,Site, Year, PPT14,CumGDD,Tmean7) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()

## looking at the data
toy.df %>% 
  ggplot(aes(x=DOY,y=Count/TrapHours, color=Year)) +geom_point()+
  facet_wrap(~Site)



## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)
library(gamm4)

toy.df <- filter(full.df, SciName== "Aedes vexans" & Domain == "D09")

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Site <- as.factor(toy.df$Site)

gam1 <- gamm4( Count ~ s(DOY,by=interaction(fYear,Site))+ offset(log(TrapHours)),
               random = ~ (1|Plot),
               data=toy.df, family="poisson")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 77:300
fYear <- c(levels(toy.df$fYear))
plot.m <- c(levels(as.factor(toy.df$Site)))

dum.df <- unique(select(toy.df, c("Plot","Site")))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)

dum.df <- unique(select(toy.df, c("Site", "Plot")))

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site),DOY)

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site,DOY),fYear)

#dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),toy.df$Site)
dum.df$TrapHours <- 12

dum.df$Pred <- predict(gam1$gam, newdata = dum.df)


dum.df$Pred[dum.df$Site=="DCFS" & dum.df$fYear=="2017"] <- NA
#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")
dum.df %>% filter( Site =="WOOD") %>% 
ggplot( aes(x=DOY, y=exp(Pred)/TrapHours,
                    color=fYear))+ 
  geom_point(data=toy.df, aes(y=Count/TrapHours,
                              x=DOY),
             color="black",size=2,alpha=.55)+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")

vexans.df <- dum.df %>% filter( Site =="WOOD") 
names(vexans.df)

colnames(vexans.df)[7] <- "Count"



## lets look atthe other two species starting with Coquillettidia


## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)
library(gamm4)

toy.df <- filter(full.df, SciName== "Coquillettidia perturbans" & Domain == "D09")

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Site <- as.factor(toy.df$Site)

gam1 <- gamm4( Count ~ s(DOY,by=interaction(fYear,Site))+ offset(log(TrapHours)),
               random = ~ (1|Plot),
               data=toy.df, family="poisson")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 77:300
fYear <- c(levels(toy.df$fYear))
plot.m <- c(levels(as.factor(toy.df$Site)))

dum.df <- unique(select(toy.df, c("Plot","Site")))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)

dum.df <- unique(select(toy.df, c("Site", "Plot")))

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site),DOY)

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site,DOY),fYear)

#dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),toy.df$Site)
dum.df$TrapHours <- 12

dum.df$Pred <- predict(gam1$gam, newdata = dum.df)


#dum.df$Pred[dum.df$Site=="DCFS" & dum.df$fYear=="2017"] <- NA
#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")
dum.df %>% #filter( Site =="WOOD") %>% 
  ggplot( aes(x=DOY, y=exp(Pred)/TrapHours,
              color=fYear))+ 
  geom_point(data=toy.df, aes(y=Count/TrapHours,
                              x=DOY),
             color="black",size=2,alpha=.55)+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")

perturbans.df <- dum.df %>% filter( Site =="WOOD") 
names(perturbans.df)

colnames(perturbans.df)[7] <- "Count"

## lets look atthe other two species starting with Coquillettidia


## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)
library(gamm4)

toy.df <- filter(full.df, SciName== "Culex tarsalis" & Domain == "D09")

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Site <- as.factor(toy.df$Site)

gam1 <- gamm4( Count ~ s( (DOY),by=interaction(fYear,Site))+ offset(log(TrapHours)),
               random = ~ (1|Plot),
               data=toy.df, family="poisson")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 77:300
fYear <- c(levels(toy.df$fYear))
plot.m <- c(levels(as.factor(toy.df$Site)))

dum.df <- unique(select(toy.df, c("Plot","Site")))

dum.df <- expand.grid(DOY,fYear)

colnames(dum.df) <- c("DOY", "fYear")

dum.df <- as.data.frame(dum.df)

dum.df <- unique(select(toy.df, c("Site", "Plot")))

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site),DOY)

dum.df <- tidyr::expand( dum.df, nesting(Plot,Site,DOY),fYear)

#dum.df <- tidyr::expand( dum.df, nesting(DOY, fYear),toy.df$Site)
dum.df$TrapHours <- 12

dum.df$Pred <- predict(gam1$gam, newdata = dum.df)


#dum.df$Pred[dum.df$Site=="DCFS" & dum.df$fYear=="2017"] <- NA
#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")
dum.df %>%# filter( Site =="WOOD") %>% 
  ggplot( aes(x=DOY, y=exp(Pred)/TrapHours),
              color=fYear)+ 
  geom_point(data= toy.df, aes(y=Count/TrapHours,
                              x=DOY,
             color=fYear),size=2,alpha=.55)+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")

tarsalis.df <- dum.df %>% filter( Site =="WOOD") 
names(tarsalis.df)

colnames(tarsalis.df)[7] <- "Count"

## combining all count data

count.df <- rbind.data.frame(vexans.df,perturbans.df, tarsalis.df)



temp.df <- filter( temp.df, fYear != '2013' & fYear!="2015")

count.df <- count.df  %>%  group_by(fYear) %>% 
  mutate( mCount = max(exp(Count)/TrapHours))


temp.df <- temp.df  %>%  group_by(fYear) %>% 
  mutate( mTemp = max(Pred))

temp.df$maxTemp <- max(temp.df$Pred)
count.df$maxCount <- max(exp(count.df$Count)/count.df$TrapHours)

ggplot() +
  geom_line(data= count.df, aes(x = DOY , y=(exp(Count)/TrapHours)/maxCount ), 
            size=2, color="#003f5c")+
  geom_line( data= temp.df, aes(x= DOY, y= Pred/maxTemp) ,color="#ffa600",size=2)+
  facet_wrap(~fYear) 


ppt.df <- filter( ppt.df, fYear != '2013' & fYear!="2015")
ggplot() +
  geom_line(data= count.df, aes(x = DOY , y=(exp(Count)/TrapHours) ), size=2, 
            color="#003f5c")+
  geom_line( data= ppt.df, aes(x= DOY, y= Pred/6) ,color="#ffa600",size=2)+
  facet_wrap(~fYear)

photo.df <- filter( photo.df, fYear != '2013' & fYear!="2015")
ggplot() +
  geom_line(data= count.df, aes(x = DOY , y=(exp(Count)/TrapHours) ), size=2, color="Navy")+
  geom_line( data= photo.df, aes(x= DOY, y= Pred/2) ,color="Green",size=2)+
  facet_wrap(~fYear)


gdd.df <- filter(gdd.df, fYear != '2013' & fYear!="2015")
ggplot() +
  geom_line(data= count.df, aes(x = DOY , y=(exp(Count)/TrapHours)/maxCount ),
            size=2, color="#003f5c")+
  geom_line( data= gdd.df, aes(x= DOY, y= Pred/max(Pred)) ,color="#ffa600",size=2)+
  facet_wrap(~fYear)

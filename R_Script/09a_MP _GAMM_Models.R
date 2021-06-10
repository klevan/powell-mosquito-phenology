####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 09_MP_GAMM_models

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory
#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )
library( rstan )
library( bayesplot )
library( rstanarm )

# input combined prism data downloaded and merged in "03_MP_PRISM_Data.R" 

# Daily Prism data
load("./Data/DailyPrismMod.Rda")

# Count data to get the domains and lat and long information
load("./Data/Mosquito_Data_Clean.Rda")

# data structure for the daily data set

str(contigus.df)

str(complete.df)

#################### Combining data sets #############

# selecting the needed columns and data to merge with count data
cont.df <- contigus.df %>% dplyr::select( -c("Lat", "Long", "Date"))

# Joining data sets so that prism data is now linked to count data
full.df <- left_join(complete.df, cont.df, by=c("Plot","Year","DOY"))

## Number of rows should match complete.df of 620662
dim(full.df) # 619554 X 34



ggplot(full.df, aes(x= CumGDD, y= log10( (Count/TrapHours)+1 )))+ geom_point()
############ subsetting to the single species, site and year #########

# We are only interested in Aedes vexans from WOOD site
toy.df <- filter(full.df, SciName== "Aedes vexans" & Domain == "D09")

unique(toy.df$Year)

## summarize to DOY level
 toy.df <- toy.df %>% #filter(Plot == "WOOD_039") %>%
   group_by(TrapEvent,Plot,Site, Year, PPT14,CumGDD,Tmean7) %>%
   summarise(Count = sum(Count),
             Count_adj = sum(Count_adj),
             TrapHours= sum(TrapHours),
             DOY = min(DOY)) %>% ungroup()




 
## looking at the data
toy.df %>% 
  ggplot(aes(x=DOY,y=Count/TrapHours, color=Year)) +geom_point()+
  facet_wrap(~Site)



## first lets try to fit a frequentist approach to verify that our bayesian
## model is working ok
library(lme4)
library(gamm4)

toy.df$fYear <- as.factor(toy.df$Year)
toy.df$Site <- as.factor(toy.df$Site)

toy.df$Count_adj <- as.integer(round(toy.df$Count_adj, 0))

toy.df$Count[is.na(toy.df$Count_adj)]

gam1 <- gam( Count_adj ~ te(DOY,by=interaction(fYear,Site))+ offset(log(TrapHours)),
             
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

dum.df$Pred <- predict(gam1, newdata = dum.df)


#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")
ggplot( dum.df, aes(x=DOY, y=exp(Pred)/TrapHours,
                    color=fYear))+ 
  geom_point(data=toy.df, aes(y=Count_adj/TrapHours,
                              x=DOY),
             color="black",size=2,alpha=.55)+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")


at1 <- toy.df %>% filter(Site=="WOOD" )
dum.df %>% filter(Site=="WOOD" ) %>% 
ggplot( aes(x=DOY, y= log10((exp(Pred)/TrapHours) +1),
                    color=fYear))+ 
  geom_point(data=at1, aes(y= log10((Count_adj/TrapHours)+1),
                              x=DOY),
             color="black",size=2,alpha=.55)+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~fYear)
  xlab("DOY") + ylab("Adult A. vexans density")+
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

  




gam1 <- gamm4( Tmean7 ~ s(DOY,by=interaction(fYear,Site)),
               random = ~ (1|Plot),
               data=toy.df, family="gaussian")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 80:300
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

dum.df$Pred <- predict(gam1$gam, newdata = dum.df)


#dum.df$Pred[dum.df$DOY < 125 ] <- NA
#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")

dum.df %>% filter(Site=="WOOD" & fYear=="2017") %>% 
ggplot(  aes(x=DOY, y=(Pred),
                    color=fYear))+ 
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")





gam1 <- gamm4( PPT14 ~ s(DOY,by=interaction(fYear,Site)),
               random = ~ (1|Plot),
               data=toy.df, family="gaussian")

summary(gam1$gam)
## lets try and plot this with new data

DOY <- 80:300
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

dum.df$Pred <- predict(gam1$gam, newdata = dum.df)


#dum.df$Pred[dum.df$DOY < 125 ] <- NA
#dum.df <- filter(dum.df, fYear != '2017' & Site != "DCFS")
at1 <- toy.df %>% filter(Site=="WOOD" & fYear=="2017")
dum.df %>% filter(Site=="WOOD" & fYear=="2017") %>% 
  ggplot(  aes(x=DOY, y=(Pred),
               color=fYear))+ 
  geom_point( data=at1, aes(x= DOY, y=PPT14))+
  geom_line(size=2,alpha=.75)+ theme_classic()+
  facet_wrap(~Site, scales="free_y")





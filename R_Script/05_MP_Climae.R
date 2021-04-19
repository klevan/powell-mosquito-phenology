####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/19/2021
# title: 05_MP_Climate_Variation

# The goal of the following script is to assess climate deviation from
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


# Count data to get the domains
load("./Data/combinded.Rda")

complete.df <- complete.df %>%  ungroup %>% 
  select( c("Plot","Domain","Site", "Elev"))

complete.df <- unique(complete.df)
# data strucutre

str(contigus.df)

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

# Combind data sets


compare.df <- left_join( DayMonth.df, thirty.df, by= c("Plot",
                                                       "Month") )
compare.df <- left_join(compare.df, complete.df, by="Plot")

compare.df %>% 
  ggplot(aes(x=ppt_month_30y_ave, y=PPT, color=as.factor(Domain)))+
  geom_point(size=2, alpha=.5)+geom_abline(intercept=0, slope=1)+
  facet_wrap(~Month, scales="free")


## Calculating the deviations

compare.df$TmaxDev <- (compare.df$Tmax - compare.df$Tmax_month_30y_ave) 
compare.df$TminDev <- (compare.df$Tmin - compare.df$Tmin_month_30y_ave)
compare.df$TmeanDev <- (compare.df$Tmean - compare.df$Tmean_month_30y_ave)
compare.df$PptDev <- (compare.df$PPT - compare.df$ppt_month_30y_ave)

compare.df %>% 
  ggplot(aes(x=TmaxDev, fill=as.factor(Domain)))+
  geom_density( alpha=.5)+
  facet_wrap(~Month, scales="free")+ theme(legend.position = "None")

compare.df %>% 
  ggplot(aes(x=TmaxDev, y=PptDev, color=as.factor(Domain)))+
  geom_point( alpha=.5)+ stat_smooth(method="lm", se=F)+
  facet_wrap(~Month, scales="free")+ theme(legend.position = "None")


### lets look at pivoting for precip

compare.df$MonthAbb <- month.abb[compare.df$Month]

ppt.df <- compare.df %>% group_by(Plot, Year, Site) %>% filter(Year>2013) %>% 
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

at1 <- FactoMineR::PCA(ppt.df[,4:15])

ppt.df$PCA1 <- at1$ind$coord[,1]
ppt.df$PCA2 <- at1$ind$coord[,2]


ggplot( ppt.df, aes(x =PCA1, y= PCA2, color=Site)) + geom_point() +
  facet_wrap(~Year)+
  theme_classic()+ geom_hline(yintercept = 0) + geom_vline(xintercept = 0)



tmean.df <- compare.df %>% group_by(Plot, Year, Site) %>% filter(Year>2013) %>% 
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

at1 <- FactoMineR::PCA(tmean.df[,4:15])

tmean.df$PCA1 <- at1$ind$coord[,1]
tmean.df$PCA2 <- at1$ind$coord[,2]


ggplot( tmean.df, aes(x =PCA1, y= PCA2, color=Site)) + geom_point() +
 #facet_wrap(~Year)+
  theme_classic()+ geom_hline(yintercept = 0) + geom_vline(xintercept = 0)



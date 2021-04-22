####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/02/2021
# title: 02_MP_Data_Exploration

# The goal of the following script is to plot some similar trends 

# load required libraries for plotting and manipulations

library( dplyr)
library( tidyr )
library( ggplot2 )

## Set working directory


setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

# input combined dataset created in "01_MP_Data_Clean_Up.R"

load("./Data/combinded.Rda")

###  Data structure ##

dim(complete.df) # 620282 X 19

names(complete.df)

str(complete.df)

## Breakdown sampling events

length(unique(complete.df$Domain)) # 20
length(unique(complete.df$Site)) # 47
length(unique(complete.df$Plot)) # 494

# summary of plots and sites within each domain

sum.df <- complete.df %>%  group_by(Domain) %>% 
  summarize( nSites= length(unique(Site))
             )

sum.df <- complete.df %>%  group_by(Domain, Site) %>% 
  summarize( nSites= length(unique(Plot))
  )

sum.df <- complete.df %>%  filter(Year!="2020") %>% group_by(Domain,Year,Plot) %>% 
  summarize( nSampling = length(unique(SampID)),
             nDates = length(unique(DOY)))
  
sum.df <- sum.df %>% group_by(Plot) %>% 
  summarize( nSampling = mean(nSampling),
             nDates = mean(nDates))

# checking to see how many sampling events we have per day
complete.df %>% group_by(Domain,Year, DOY,SciName) %>%
  summarise(nDates= length(unique(SampID)),
            nPlots = length(unique( Plot ))) %>% 
  ggplot(aes(x=nDates/nPlots, fill = Year))+geom_density(position = "stack") +theme_classic()+
  xlab("Number of sampling events per plot") + theme(legend.position = c(.8,.7))




## Lets select taxa that we said we would model 

grant.df <- complete.df %>% filter( SciName == "Aedes canadensis" |
                                    SciName == "Aedes communis" |
                                    SciName ==  "Aedes vexans" |
                                    SciName == "Coquillettidia perturbans"|
                                    SciName ==  "Culex salinarius" |
                                    SciName ==  "Culex erraticus"|
                                    SciName ==   "Culex tarsalis"  )


### Lets check the new dimenstions and add simple trait data from grant

dim(grant.df) #  130804 X 16

trait.df <- read.csv("./Data/Simple_Trait.csv")

grant.df <- left_join(grant.df, trait.df, by = "SciName")

dim(grant.df) # 130804  X 20

sum.df <-grant.df %>% group_by(Domain, Year, NorD, Plot) %>%
  summarise(nSamp = length(unique(SampID)))

ggplot(sum.df, aes(x=nSamp,fill=NorD))+geom_density(alpha=.5)+
  facet_wrap(~Domain,scales="free_y")

# adding a column for total collected taxa
grant.df <- grant.df %>% 
  group_by(SciName, Year, Plot) %>% 
  mutate( nTotal = sum(Count)) %>% ungroup()


grant.df %>%  filter(nTotal >0 ) %>% 
  ggplot(aes(x = log10(nTotal), fill=Domain)) +geom_density(alpha=.5)+
  facet_wrap(~SciName, scales="free") 


grant.df %>%  filter(nTotal >0 ) %>% 
  ggplot(aes(y = log10(nTotal), fill=Year, x=Domain)) +geom_boxplot(alpha=.5)+
  facet_wrap(~SciName, scales="free") + theme_classic()+ 
  xlab("Domains") +ylab("log10(Mosquito densities)")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

  

grant.df %>%  filter(nTotal >0 ) %>% 
  ggplot(aes(y = log10(nTotal), x=Year, fill=SciName)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free") + theme_classic()+
  xlab("Year") +ylab("log10(Mosquito densities)")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position =c(.91,.1),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black"),
         axis.text.y  = element_text(vjust=0.5,color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )





grant.df %>%  filter(Count >0 ) %>% 
  ggplot(aes(y = log10( (Count/TrapHours)+1), color=Domain, x=DOY)) +geom_point(alpha=.5)+
  facet_wrap(~SciName, scales="free") 

## Domain 6


grant.df %>% filter(Domain == "D06" & SciName == "Aedes vexans" & Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=Year)) +geom_point(alpha=.5)+
  facet_wrap(~Site, scales="free_y", ncol=1) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Domain == "D06" & SciName == "Aedes vexans" ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", ncol=1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



grant.df %>% filter(Site == "UKFS" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "UKFS"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c( "#9999CC","#CC6666", "#66CC99", "#cc66cc", "#ff9933",
                                 "#ff9999") )+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



## Domain 8

grant.df %>% filter(Domain == "D08" & SciName == "Aedes vexans" & Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=Year)) +geom_point(alpha=.5)+
  facet_wrap(~Site, scales="free_y", ncol=1) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Domain == "D08" & SciName == "Aedes vexans" ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", ncol=1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



grant.df %>% filter(Site == "DELA" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "DELA"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c( "#9999CC","#CC6666", "#66CC99", "#cc66cc", "#ff9933") )+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )





View(grant.df %>% filter(Domain == "D06"& SciName == "Aedes vexans" & 
                      Year == "2017" & Plot =="UKFS_037") )



###### Domain 9 


grant.df %>% filter(Domain == "D09" & SciName == "Aedes vexans" ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", ncol=1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

grant.df %>% filter(Domain == "D09" & SciName == "Aedes vexans" & Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=Year)) +geom_point(alpha=.5)+
  facet_wrap(~Site, scales="free_y", ncol=1) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



## Site WOOD

grant.df %>% filter(Site == "WOOD" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "WOOD"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c("#CC6666", "#9999CC", "#66CC99"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

## SITE NOGP

grant.df %>% filter(Site == "NOGP" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "WOOD"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c("#CC6666", "#9999CC", "#66CC99", "#cc66cc"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

## SITE DCFS

grant.df %>% filter(Site == "DCFS" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "DCFS"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c("#CC6666", "#9999CC", "#66CC99", "#cc66cc"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


###### Domain 9 


grant.df %>% filter(Domain == "D05" & SciName == "Aedes vexans" ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", ncol=1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

grant.df %>% filter(Domain == "D05" & SciName == "Aedes vexans" & Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=Year)) +geom_point(alpha=.5)+
  facet_wrap(~Site, scales="free_y", ncol=1) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



## Site STEI

grant.df %>% filter(Site == "STEI" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "STEI"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c( "#9999CC", "#66CC99", "#CC6666","#cc66cc"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


## Site TREE

grant.df %>% filter(Site == "TREE" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "TREE"  & Year != "2020" &
                      SciName != "Culex erraticus") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c( "#9999CC", "#66CC99", "#CC6666","#cc66cc"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

## Site UNDE

grant.df %>% filter(Site == "UNDE" & SciName == "Aedes vexans"& Year != "2020" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) , color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=5) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#CC6666")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "UNDE"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=5) + stat_smooth(size=2,se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = c( "#9999CC", "#66CC99", "#CC6666","#cc66cc"))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )




###### Domain 14 ( Desert )

grant.df %>% filter(Domain == "D14"  ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", nco =1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



grant.df %>% filter(Site == "SRER" ) %>% 
  group_by(DOY, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)) )) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#C59900")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "DCFS"  & Year != "2020") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName)) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position = "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



###### Domain 11 ( Desert )

grant.df %>% filter(Domain == "D11"  ) %>% 
  group_by(Domain, Site, Year, Plot) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = Year, y=( ( (Count/TrapHours)) ), fill=Site)) +geom_boxplot(alpha=.5)+
  facet_wrap(~Domain, scales="free_y", nco =1) +  theme_classic()+
  xlab("Year") +ylab("Mosquito densities")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "top",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )



grant.df %>% filter(Site == "OAES"& SciName == "Aedes vexans" ) %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName )) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  scale_color_manual(values = "#C59900")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


grant.df %>% filter(Site == "OAES") %>% 
  group_by(DOY, Site, Year, Plot, SciName) %>%
  summarise( Count=sum(Count), TrapHours= sum(TrapHours)) %>% 
  ggplot(aes(x = DOY, y=( (Count/TrapHours)), color=SciName )) +geom_point(alpha=.5)+
  facet_wrap(~Year, scales="free_y", ncol=4) + stat_smooth(se = F)+ theme_classic()+
  xlab("Day of Year") +ylab("Mosquito densities")+
  #scale_color_manual(values = "#C59900")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.position= "none",
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )

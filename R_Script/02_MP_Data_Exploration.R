####### Powell Center: Phenological patterns of mosquitos #######

# Travis McDevitt-Galles
# 04/02/2021
# title: 02_MP_Data_Exploration

# The goal of the following script is to plot some similare trends 

# load required libraries for plotting and manipulationss

library( dplyr)
library( tidyr )
library( ggplot2 )

## Set working directory

setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

# input combinded dataset created in "01_MP_Data_Clean_Up.R"

load("./Data/combinded.Rda")

###  Data strucutre ##

dim(complete.df) # 2068306 x 26

names(complete.df)

str(complete.df)

## Lets select taxa that we said we would model 

grant.df <- complete.df %>% filter( SciName == "Aedes canadensis" |
                                    SciName == "Aedes communis" |
                                    SciName ==  "Aedes vexans" |
                                    SciName == "Coquillettidia perturbans"|
                                    SciName ==  "Culex salinarius" |
                                    SciName ==  "Culex erraticus"|
                                    SciName ==   "Culex tarsalis"  )


### Lets check the new dimenstions and add simple trait data from grant

dim(grant.df) #  376600 X 26

trait.df <- read.csv("./Data/Simple_Trait.csv")

grant.df <- left_join(grant.df, trait.df, by = "SciName")

dim(grant.df) # 376600 x 31

# adding a column for total collected taxa
grant.df <- grant.df %>% 
  group_by(SciName, Year, Plot) %>% 
  mutate( nTotal = sum(Count)) %>% ungroup()


grant.df %>%  filter(nTotal >0 ) %>% 
  ggplot(aes(x = log10(nTotal), fill=Domain)) +geom_density(alpha=.5)+
  facet_wrap(~SciName, scales="free") 


grant.df %>%  filter(nTotal >0 ) %>% 
  ggplot(aes(y = log10(nTotal), fill=Year, x=Domain)) +geom_boxplot(alpha=.5)+
  facet_wrap(~SciName, scales="free") 


grant.df %>% filter(Domain == "D03" & SciName == "Aedes vexans" & Plot == "DSNY_080") %>% 
  ggplot(aes(x = DOY, y=Count/TrapHours, color=Year)) +geom_point(alpha=.5)+
  facet_wrap(~Plot, scales="free") 

View(grant.df %>% filter(Domain == "D03" & SciName == "Aedes vexans" & Plot == "DSNY_080"))

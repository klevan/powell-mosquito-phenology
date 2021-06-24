####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 06/10/2021
# title: 11_MP_Spp_Explore

# Creating species specific phenology patterns and quantifying varition both
# across location and time to get a better sense of phenological sensitivies

#Set working directory
#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

library( dplyr ) 
library( tidyr )
library( ggplot2 )
library(gamm4)
library(ggridges)

# Count data to get the domains and lat and long information
load("./Data/Mosquito_Data_Clean.Rda")

# data structure for the daily data set

str(complete.df)



################################################################################
#######################    AEDES VEXANS      ###################################
################################################################################



vexans.df <- filter( complete.df, SciName == "Aedes vexans")


dim(vexans.df) # 33354 X 21

## summarize data down to DOY level ( combinding Night and Day)

vexans.df <- vexans.df %>%  group_by( Domain, Site,Year, TrapEvent,Plot) %>% 
            summarise(
              Count = sum(Count),
              Count_adj = sum(Count_adj),
              TrapHours = sum(TrapHours),
              Lat = mean(Lat),
              Long = mean(Long),
              Elev = mean(Elev),
              DOY = min(DOY)
            ) %>% ungroup()

## Ok now we just plot some data, lets check out abudnace trends over different
## domains

vexans.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x= Domain, fill=Domain) )+
            geom_boxplot(alpha=.5) + theme_classic() +
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#( "vexans_ABUND.png", width=7 , height=5 , units="in")



## Ok now lets get some simple modeling of the phenology
pheno.df <- vexans.df %>% filter(  Site == "WOOD" | # D 9 site
                                   Site == "UNDE" | # D 8 site
                                   Site == "UKFS"   # D 6 site
                                   ) %>% 
  group_by( Domain, Site, DOY,Year, Plot ) %>% 
  summarise(
    Count_adj = round(sum(Count_adj),0),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

## Checkign to see what year has the most data

pheno.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Domain, color=Domain) )+
  geom_point(size=4,alpha=.5) + theme_classic() +
  facet_wrap(~Year)+
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

## 2016 has the most difference across the three domains

model.df <- pheno.df %>% filter( Year == 2018)

vex.gam <- gam( Count_adj ~ te(DOY,by=interaction(Site))+ offset(log(TrapHours)),
               #random = ~ (1|Plot),
               data=model.df, family="poisson")

summary(vex.gam)

## lets get the predicted fit

DOY <-74:321
site.m <- c(levels(as.factor(model.df$Site)))

dum.df <- expand.grid(DOY,site.m)

colnames(dum.df) <- c("DOY", "Site")

dum.df <- as.data.frame(dum.df)

dum.df$TrapHours <- 12

dum.df$Pred <- predict(vex.gam, newdata = dum.df)

ggplot(dum.df , aes(x=DOY, y= exp(Pred)/TrapHours, color=Site))+
  geom_line()+ facet_wrap(~Site, scales="free")

ggplot(dum.df , aes(x=DOY, y =( exp(Pred)/TrapHours), group=Site))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)

dum.df  %>% 
  ggplot(aes(x=DOY, fill= exp(Pred)/TrapHours,  y= Site)) +
  geom_ridgeline_gradient(aes(height=log10( (exp(Pred)/TrapHours) +1)
                             ),
                          alpha=.1,size=1,scale = 2) + 
  theme_classic() +
  scale_fill_viridis_c(name = "Mosquito density", option = "C")+ 

  xlab("Day of year") +ylab("Site")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.8),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "vexans_pheno.png", width=8 , height=5 , units="in")


## vexans across time and space 

## lets first for loop it to get proportions

var.df <- vexans.df %>%  group_by( Domain, Site,  DOY, Year) %>% 
  summarise(
   Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

var.df$Count_pro <- NA

t <-1

for( s in 1:length(unique(var.df$Site))){
  foc_site <- unique(var.df$Site)[s]
  foc.df <- filter(var.df, Site == foc_site)
  
  for( y in 1:length(unique(foc.df$Year))){
    foc_year <- unique(foc.df$Year)[y]
    
    year.df <- filter( foc.df, Year == foc_year)
    
    year.df$Count_pro <- (year.df$Count_adj/year.df$TrapHours)/
      max(year.df$Count_adj/year.df$TrapHours)
   if( t == 1){
     new.df <- year.df
     t <- t + 1
   }else(
     new.df <- rbind.data.frame(new.df, year.df)
   )
  }
}


new.df$Count_pro <- as.numeric(new.df$Count_pro)
new.df$eighty <- "0"
new.df$eighty[new.df$Count_pro >= .2] <- '1'
new.df <- new.df[!is.na(new.df$eighty),]


pres.df <- filter(new.df, eighty == '1' )



pres.df <- pres.df %>% group_by(Site) %>% 
           mutate( nYear = length(unique(Year)))

pres.df$exclude  <- 0

pres.df$exclude[pres.df$nYear >3 ] <- 1

pres.df <- filter( pres.df, exclude == 1)
max.df <- filter( pres.df, Count_pro ==1 )

ggplot( pres.df, aes(x=DOY, color= Year, y=as.factor(Year)))+ geom_line(size=2)+
  geom_point(data=max.df, aes(x=DOY, y= as.factor(Year) ),color="black",size=1)+
  facet_wrap(~Site, scales= "free_y",ncol =5)+
  theme_classic()+ ylab("Day of Year") + xlab("Year")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=12),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=12),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=16) )

ggsave( "vexans_stab.png", width=10 , height=7.5 , units="in")



################################################################################
#######################    Aedes canadensis  ###################################
################################################################################


canad.df <- filter( complete.df, SciName == "Aedes canadensis")


dim(canad.df) # 14192 X 21

## summarize data down to DOY level ( combinding Night and Day)

canad.df <- canad.df %>%  group_by( Domain, Site,Year, TrapEvent,Plot) %>% 
  summarise(
    Count = sum(Count),
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat),
    Long = mean(Long),
    Elev = mean(Elev),
    DOY = min(DOY)
  ) %>% ungroup()

## Ok now we just plot some data, lets check out abudnace trends over different
## domains

canad.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% filter( Domain == "D05" | Domain =="D01" | Domain == "D08") %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x= Site, fill=Domain) )+
  geom_boxplot(alpha=.5) + theme_classic() +
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         # legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "Canad_ABUND.png", width=7 , height=5 , units="in")



## Ok now lets get some simple modeling of the phenology
pheno.df <- canad.df %>% filter(  Site == "STEI" | # D 5 site
                                    Site == "HARV" | # D 1 site
                                    Site == "TALL"   # D 8 site
) %>% 
  group_by( Domain, Site, DOY,Year, Plot ) %>% 
  summarise(
    Count_adj = round(sum(Count_adj),0),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

## Checkign to see what year has the most data

pheno.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Domain, color=Domain) )+
  geom_point(size=4,alpha=.5) + theme_classic() +
  facet_wrap(~Year)+
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

## 2016 has the most difference across the three domains

model.df <- pheno.df %>% filter( Year == 2018)

can.gam <- gam( Count_adj ~ te(DOY,by=interaction(Site))+ offset(log(TrapHours)),
                #random = ~ (1|Plot),
                data=model.df, family="poisson")

summary(can.gam)

## lets get the predicted fit

DOY <-2:364
site.m <- c(levels(as.factor(model.df$Site)))

dum.df <- expand.grid(DOY,site.m)

colnames(dum.df) <- c("DOY", "Site")

dum.df <- as.data.frame(dum.df)

dum.df$TrapHours <- 12

dum.df$Pred <- predict(can.gam, newdata = dum.df)
dum.df$Pred[dum.df$DOY > 300]  <- -100000
ggplot(dum.df , aes(x=DOY, y= exp(Pred)/TrapHours, color=Site))+
  geom_line()+ facet_wrap(~Site, scales="free")

ggplot(dum.df , aes(x=DOY, y =( exp(Pred)/TrapHours), group=Site))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)

dum.df  %>% 
  ggplot(aes(x=DOY, fill= exp(Pred)/TrapHours,  y= Site)) +
  geom_ridgeline_gradient(aes(height=log10( (exp(Pred)/TrapHours) +1)
  ),
  alpha=.1,size=1,scale = 3) + 
  theme_classic() +
  scale_fill_viridis_c(name = "Mosquito density", option = "C")+ 
  
  xlab("Day of year") +ylab("Site")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.8),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "cand_pheno.png", width=8 , height=5 , units="in")


## Canada across time and space 

## lets first for loop it to get proportions

var.df <- canad.df %>%  group_by( Domain, Site,  DOY, Year) %>% 
  summarise(
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

var.df$Count_pro <- NA

t <-1

for( s in 1:length(unique(var.df$Site))){
  foc_site <- unique(var.df$Site)[s]
  foc.df <- filter(var.df, Site == foc_site)
  
  for( y in 1:length(unique(foc.df$Year))){
    foc_year <- unique(foc.df$Year)[y]
    
    year.df <- filter( foc.df, Year == foc_year)
    
    year.df$Count_pro <- (year.df$Count_adj/year.df$TrapHours)/
      max(year.df$Count_adj/year.df$TrapHours)
    if( t == 1){
      new.df <- year.df
      t <- t + 1
    }else(
      new.df <- rbind.data.frame(new.df, year.df)
    )
  }
}


new.df$Count_pro <- as.numeric(new.df$Count_pro)
new.df$eighty <- "0"
new.df$eighty[new.df$Count_pro >= .2] <- '1'
new.df <- new.df[!is.na(new.df$eighty),]


pres.df <- filter(new.df, eighty == '1' )



pres.df <- pres.df %>% group_by(Site) %>% 
  mutate( nYear = length(unique(Year)))

pres.df$exclude  <- 0

pres.df$exclude[pres.df$nYear >3 ] <- 1

pres.df <- filter( pres.df, exclude == 1)
max.df <- filter( pres.df, Count_pro ==1 )

ggplot( pres.df, aes(x=DOY, color= Year, y=as.factor(Year)))+ geom_line(size=2)+
  geom_point(data=max.df, aes(x=DOY, y= as.factor(Year) ),color="black",size=1)+
  facet_wrap(~Site, scales= "free_y",ncol =5)+
  theme_classic()+ ylab("Day of Year") + xlab("Year")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=12),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=12),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=16) )

ggsave( "cand_stab.png", width=10 , height=7.5 , units="in")


################################################################################
#######################    Aedes communis    ###################################
################################################################################

communis.df <- filter( complete.df, SciName == "Aedes communis")


dim(communis.df) #8980 X 21

## summarize data down to DOY level ( combinding Night and Day)

communis.df <- communis.df %>%  group_by( Domain, Site,Year, TrapEvent,Plot) %>% 
  summarise(
    Count = sum(Count),
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat),
    Long = mean(Long),
    Elev = mean(Elev),
    DOY = min(DOY)
  ) %>% ungroup()

## Ok now we just plot some data, lets check out abudnace trends over different
## domains

communis.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% filter( Domain == "D05" | Domain =="D13" | Domain == "D01") %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x= Site, fill=Domain) )+
  geom_boxplot(alpha=.5) + theme_classic() +
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "Comm_ABUND.png", width=7 , height=5 , units="in")



## Ok now lets get some simple modeling of the phenology
pheno.df <- communis.df %>% filter(  Site == "NIWO" | # D 5 site
                                       Site == "STEI" | # D 1 site
                                       Site == "UNDE"   # D 8 site
) %>% 
  group_by( Domain, Site, DOY,Year, Plot ) %>% 
  summarise(
    Count_adj = round(sum(Count_adj),0),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

## Checkign to see what year has the most data

pheno.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Domain, color=Domain) )+
  geom_point(size=4,alpha=.5) + theme_classic() +
  facet_wrap(~Year)+
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

## 2016 has the most difference across the three domains

model.df <- pheno.df %>% filter( Year == 2019)

com.gam <- gam( Count_adj ~ te(DOY,by=interaction(Site))+ offset(log(TrapHours)),
                #random = ~ (1|Plot),
                data=model.df, family="poisson")

summary(com.gam)

## lets get the predicted fit

DOY <-99:310
site.m <- c(levels(as.factor(model.df$Site)))

dum.df <- expand.grid(DOY,site.m)

colnames(dum.df) <- c("DOY", "Site")

dum.df <- as.data.frame(dum.df)

dum.df$TrapHours <- 12

dum.df$Pred <- predict(com.gam, newdata = dum.df)
#dum.df$Pred[dum.df$DOY > 300]  <- -100000
ggplot(dum.df , aes(x=DOY, y= exp(Pred)/TrapHours, color=Site))+
  geom_line()+ facet_wrap(~Site, scales="free")

ggplot(dum.df , aes(x=DOY, y =( exp(Pred)/TrapHours), group=Site))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)

dum.df  %>% 
  ggplot(aes(x=DOY, fill= exp(Pred)/TrapHours,  y= Site)) +
  geom_ridgeline_gradient(aes(height=log10( (exp(Pred)/TrapHours) +1)
  ),
  alpha=.1,size=1,scale = 3) + 
  theme_classic() +
  scale_fill_viridis_c(name = "Mosquito density", option = "C")+ 
  
  xlab("Day of year") +ylab("Site")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.8),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "com_pheno.png", width=8 , height=5 , units="in")


## Canada across time and space 

## lets first for loop it to get proportions

var.df <- communis.df %>%  group_by( Domain, Site,  DOY, Year) %>% 
  summarise(
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

var.df$Count_pro <- NA

t <-1

for( s in 1:length(unique(var.df$Site))){
  foc_site <- unique(var.df$Site)[s]
  foc.df <- filter(var.df, Site == foc_site)
  
  for( y in 1:length(unique(foc.df$Year))){
    foc_year <- unique(foc.df$Year)[y]
    
    year.df <- filter( foc.df, Year == foc_year)
    
    year.df$Count_pro <- (year.df$Count_adj/year.df$TrapHours)/
      max(year.df$Count_adj/year.df$TrapHours)
    if( t == 1){
      new.df <- year.df
      t <- t + 1
    }else(
      new.df <- rbind.data.frame(new.df, year.df)
    )
  }
}


new.df$Count_pro <- as.numeric(new.df$Count_pro)
new.df$eighty <- "0"
new.df$eighty[new.df$Count_pro >= .2] <- '1'
new.df <- new.df[!is.na(new.df$eighty),]


pres.df <- filter(new.df, eighty == '1' )



pres.df <- pres.df %>% group_by(Site) %>% 
  mutate( nYear = length(unique(Year)))

pres.df$exclude  <- 0

pres.df$exclude[pres.df$nYear >3 ] <- 1

pres.df <- filter( pres.df, exclude == 1)
max.df <- filter( pres.df, Count_pro ==1 )

ggplot( pres.df, aes(x=DOY, color= Year, y=as.factor(Year)))+ geom_line(size=2)+
  geom_point(data=max.df, aes(x=DOY, y= as.factor(Year) ),color="black",size=1)+
  facet_wrap(~Site, scales= "free_y",ncol =3)+
  theme_classic()+ ylab("Day of Year") + xlab("Year")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=12),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=12),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=16) )

#ggsave( "comm_stab.png", width=10 , height=7.5 , units="in")



################################################################################
#######################    Co. perturbans    ###################################
################################################################################

perturban.df <- filter( complete.df, SciName == "Coquillettidia perturbans")


dim(perturban.df) #22771 X 21

## summarize data down to DOY level ( combinding Night and Day)

perturban.df <- perturban.df %>%  group_by( Domain, Site,Year, TrapEvent,Plot) %>% 
  summarise(
    Count = sum(Count),
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat),
    Long = mean(Long),
    Elev = mean(Elev),
    DOY = min(DOY)
  ) %>% ungroup()

## Ok now we just plot some data, lets check out abudnace trends over different
## domains

perturban.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% #filter( Domain == "D05" | Domain =="D13" | Domain == "D01") %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Domain , fill=Domain) )+
  geom_boxplot(alpha=.5) + theme_classic() +
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "Per_ABUND.png", width=7 , height=5 , units="in")

perturban.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% filter( Domain == "D01" | Domain =="D03" | Domain == "D05") %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Site, fill=Domain) )+
  geom_boxplot(alpha=.5) + theme_classic() +
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


## Ok now lets get some simple modeling of the phenology
pheno.df <-perturban.df %>% filter(  Site == "DSNY" | # D 5 site
                                       Site == "HARV" | # D 1 site
                                       Site == "TREE"   # D 8 site
) %>% 
  group_by( Domain, Site, DOY,Year, Plot ) %>% 
  summarise(
    Count_adj = round(sum(Count_adj),0),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

## Checkign to see what year has the most data

pheno.df %>%  group_by( Domain, Site,  Year) %>% 
  summarise(
    nTotal = sum(Count_adj),
    nHours = sum(TrapHours),
    Lat = mean(Lat)
  ) %>% 
  ggplot( aes( y = log10( ( nTotal/nHours)+1) , x=Domain, color=Domain) )+
  geom_point(size=4,alpha=.5) + theme_classic() +
  facet_wrap(~Year)+
  xlab("Domain") + ylab("log10( Total density )")+
  theme( legend.key.size = unit(1.5, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

## 2016 has the most difference across the three domains

model.df <- pheno.df %>% filter( Year == 2019)

per.gam <- gam( Count_adj ~ te(DOY,by=interaction(Site))+ offset(log(TrapHours)),
                #random = ~ (1|Plot),
                data=model.df, family="poisson")

summary(per.gam)

## lets get the predicted fit

DOY <-2:364
site.m <- c(levels(as.factor(model.df$Site)))

dum.df <- expand.grid(DOY,site.m)

colnames(dum.df) <- c("DOY", "Site")

dum.df <- as.data.frame(dum.df)

dum.df$TrapHours <- 12

dum.df$Pred <- predict(per.gam, newdata = dum.df)
dum.df$Pred[dum.df$DOY <100 & dum.df$Site == "TREE" ]  <- -100000
dum.df$Pred[dum.df$DOY > 300 & dum.df$Site == "TREE" ]  <- -100000
dum.df$Pred[dum.df$DOY <100 & dum.df$Site == "HARV" ]  <- -100000
dum.df$Pred[dum.df$DOY > 300 & dum.df$Site == "HARV" ]  <- -100000
ggplot(dum.df , aes(x=DOY, y= exp(Pred)/TrapHours, color=Site))+
  geom_line()+ facet_wrap(~Site, scales="free")

ggplot(dum.df , aes(x=DOY, y =( exp(Pred)/TrapHours), group=Site))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)

dum.df  %>% 
  ggplot(aes(x=DOY, fill= exp(Pred)/TrapHours,  y= Site)) +
  geom_ridgeline_gradient(aes(height=log10( (exp(Pred)/TrapHours) +1)
  ),
  alpha=.1,size=.5,scale = .75) + 
  theme_classic() +
  scale_fill_viridis_c(name = "Mosquito density", option = "C")+ 
  
  xlab("Day of year") +ylab("Site")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = c(.9,.8),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=14),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=14),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

#ggsave( "per_pheno.png", width=8 , height=5 , units="in")


## Canada across time and space 

## lets first for loop it to get proportions

var.df <- perturban.df %>%  group_by( Domain, Site,  DOY, Year) %>% 
  summarise(
    Count_adj = sum(Count_adj),
    TrapHours = sum(TrapHours),
    Lat = mean(Lat)
  ) 

var.df$Count_pro <- NA

t <-1

for( s in 1:length(unique(var.df$Site))){
  foc_site <- unique(var.df$Site)[s]
  foc.df <- filter(var.df, Site == foc_site)
  
  for( y in 1:length(unique(foc.df$Year))){
    foc_year <- unique(foc.df$Year)[y]
    
    year.df <- filter( foc.df, Year == foc_year)
    
    year.df$Count_pro <- (year.df$Count_adj/year.df$TrapHours)/
      max(year.df$Count_adj/year.df$TrapHours)
    if( t == 1){
      new.df <- year.df
      t <- t + 1
    }else(
      new.df <- rbind.data.frame(new.df, year.df)
    )
  }
}


new.df$Count_pro <- as.numeric(new.df$Count_pro)
new.df$eighty <- "0"
new.df$eighty[new.df$Count_pro >= .4] <- '1'
new.df <- new.df[!is.na(new.df$eighty),]


pres.df <- filter(new.df, eighty == '1' )



pres.df <- pres.df %>% group_by(Site) %>% 
  mutate( nYear = length(unique(Year)))

pres.df$exclude  <- 0

pres.df$exclude[pres.df$nYear >3 ] <- 1

pres.df <- filter( pres.df, exclude == 1)
max.df <- filter( pres.df, Count_pro ==1 )

ggplot( pres.df, aes(x=DOY, color= Year, y=as.factor(Year)))+ geom_line(size=2)+
  geom_point(data=max.df, aes(x=DOY, y= as.factor(Year) ),color="black",size=1)+
  facet_wrap(~Site, scales= "free_y",ncol =3)+
  theme_classic()+ ylab("Day of Year") + xlab("Year")+
  theme( legend.key.size = unit(.7, "cm"),
         legend.title =element_text(size=14,margin = margin(r =10, unit = "pt")),
         legend.text=element_text(size=14,margin = margin(r =10, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, color = "black",size=12),
         axis.text.y  = element_text(vjust=0.5,color = "black",size=12),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=16) )




#ggsave( "per_stab.png", width=10 , height=7.5 , units="in")

early.df <- perturban.df %>% filter( Site == "HARV")

early.df %>%group_by( Year) %>% 
  summarize(ndays = length(unique(DOY)))


ggplot(early.df , aes(x = DOY, y = log10( (Count_adj/TrapHours)+1), color=as.factor(Year)))+
  geom_point(size=2, alpha=.5) + facet_wrap(~Plot, scales = "free_y")


early.df$Count_pro <- NA


t <-1

for( s in 1:length(unique(early.df$Plot))){
  foc_site <- unique(early.df$Plot)[s]
  foc.df <- filter(early.df, Plot == foc_site)
  
  for( y in 1:length(unique(foc.df$Year))){
    foc_year <- unique(foc.df$Year)[y]
    
    year.df <- filter( foc.df, Year == foc_year)
    
    year.df$Count_pro <- (year.df$Count_adj/year.df$TrapHours)/
      max(year.df$Count_adj/year.df$TrapHours)
    if( t == 1){``
      new.df <- year.df
      t <- t + 1
    }else(
      new.df <- rbind.data.frame(new.df, year.df)
    )
  }
}


ggplot(new.df , aes(x = DOY, y = (Count_pro), color=as.factor(Year)))+
  geom_point(size=2, alpha=.5) + facet_wrap(~Plot, scales = "free_y")


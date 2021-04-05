#### Development for conceptual framework mosquito review paper ######

# Travis McDevitt-Galles
# 04/05/2021
# title: 01_RP_Concep_Diagram

# developing conceptual diagram to illustrate the knowledge gap we are trying
# to address in this Powell Center project


## thinking of developing three to four rows of potential shifts in variation
## in phenological patterns to better illustrate our goals
## Maybe break it up to two figures

library(ggplot2)
library(ggridges)
library(viridis)
library(patchwork)
### Creating theoritical phenological pattern and shifts under warming
###  temperatures

# Creating three distinc phenological patterns using dnorm

nDays <- seq(0,20, length.out = 1000)
Pheno <- c(dnorm(nDays, 6,2),dnorm(nDays, 10,2),dnorm(nDays, 14,2) )
# Creating a vector of temps assuming each phenology is tracking the same
# temperature gradiet
DOY <- rep(nDays,3)
# Vector of DOY to illustrate shifts in phenology across years
Temp <-c(cumsum(dnorm(nDays, 6,2)),
         cumsum(dnorm(nDays, 10,2)) , cumsum(dnorm(nDays, 14,2) ))
## Vector of years to make grouping easier
Year <- c(rep("1" ,1000), rep("2",1000), rep("3", 1000))

# combind vectors into one dataframe
con.df <- cbind.data.frame(DOY, Temp, Pheno, Year)

names(con.df) # checking to make sure nammes are accurate

dim(con.df) # 300 x 4

# quick plot of these phennologies 

con.a <- con.df %>% 
  ggplot(aes(x=DOY, y=-as.integer(Year),  group= Year, fill=Temp)) +
  geom_ridgeline_gradient(aes(height=Pheno*5, color=Year),
                          alpha=.1,size=3,scale = 3) + 
                           theme_classic() +
  scale_color_brewer(palette = "Set1")+
  scale_fill_viridis_c(name = "Temp. [C]", option = "C")+ 
  xlab("Day of year") +ylab("Mosquito population")+
  theme( legend.key.size = unit(1, "cm"),
         legend.title =element_text(size=18,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=18,margin = margin(r = 40, unit = "pt")), 
         legend.position ="none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



con.b <- con.df %>% 
  ggplot(aes(x=DOY, y=-as.integer(Year),  group= Year, fill=DOY)) +
  geom_ridgeline_gradient(aes(height=Pheno*5, color=Year),
                          alpha=.1,size=3,scale = 3) + 
  theme_classic() +
  scale_color_brewer(palette = "Set1")+
  scale_fill_viridis_c(name = "Temp. [C]", option = "C")+ 
  guides(linetype = FALSE, color=F) +
  theme_classic() + 
  xlab("Day of year") +ylab("Mosquito population")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position =c(.95,.8),
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )



con.a / con.b

ggsave("Concept_A.pdf", height=7,width = 5, dpi=320)


## Second part of conceptual diagram changes in experienced temperature across
## variation in emergence time


#stablishing emergence dates
Emerge <- c(2,4,6)

Temp  <-c(20,20,20)

Year <- as.factor(1:3)
consist.df <- cbind.data.frame(Emerge, Temp, Year)

con2.a <- consist.df %>% 
  ggplot(aes(x= Emerge, y= Temp) )+
  geom_line(size =2, alpha=.8, linetype=2)  +
  theme_classic() + 
  xlab("Phenological shift") +ylab("Experienced temperature")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )


#stablishing emergence dates
Emerge <- c(2,4,6)

Temp  <-c(15,20,25)

Year <- as.factor(1:3)
consist.df <- cbind.data.frame(Emerge, Temp, Year)

con2.b <- consist.df %>% 
  ggplot(aes(x= Emerge, y= Temp) )+
  geom_line(size =2, alpha=.8, linetype=2)  +
  theme_classic() + 
  xlab("∆ in phenological pattern") +ylab("Experienced temperature")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

con2.a/con2.b

ggsave("Concept_b.pdf", height=7,width = 5, dpi=320)


### Third part

# first step is to get some summary data 


# Establishing emergence dates
Emerge <- c(rep(2,26), rep(4,26) , rep(6,26))

Temp  <-c(rnorm(26,20,3),rnorm(26,20,3),rnorm(26,20,3))

Year <- as.factor(c(rep(1,26), rep(2,26) , rep(3,26)))
consist.df <- cbind.data.frame(Emerge, Temp, Year)

consist.df %>% 
  ggplot(aes(x= Emerge, y= Temp, fill=Year) )+
  geom_violin(size =1, alpha=.8)  +
  theme_classic() + 
  xlab("DOY of emergence") +ylab("Experienced temperature")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )




#stablishing emergence dates

# Establishing emergence dates
Emerge <- c(rep(2,26), rep(4,26) , rep(6,26))

Temp  <-c(rnorm(26,20,5),rnorm(26,20,5),rnorm(26,20,5))

Year <- as.factor(c(rep(1,26), rep(2,26) , rep(3,26)))
consist.df <- cbind.data.frame(Emerge, Temp, Year)

con3.a <- consist.df %>% 
  ggplot(aes(x= Emerge, y= Temp, fill=Year) )+
  geom_boxplot(size =1, alpha=.8)  +
  theme_classic() + 
  scale_fill_brewer(palette="Set1")+
  xlab("Population") +ylab("Relative disease risk")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )






Emerge <- c(rep(2,26), rep(4,26) , rep(6,26))

Temp  <-c(rnorm(26,15,5),rnorm(26,25,8),rnorm(26,16,5))

Year <- as.factor(c(rep(1,26), rep(2,26) , rep(3,26)))
consist.df <- cbind.data.frame(Emerge, Temp, Year)

con3.b <- consist.df %>% 
  ggplot(aes(x= Emerge, y= Temp, fill=Year) )+
  geom_boxplot(size =1, alpha=.8, outlier.alpha  = 0)  +
  theme_classic() + 
  scale_fill_brewer(palette="Set1")+
  xlab("Population") +ylab("Relative disease risk")+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )

con3.a/con3.b






ggsave("Concept_c.pdf", height=7,width = 5, dpi=320)


Temp <- seq(0,2.5, length.out = 1000)

Disease <- dweibull(Temp, shape =2)

disease.df <- cbind.data.frame(Temp,Disease)

con3.a <- disease.df %>% 
  ggplot( aes( x= Temp, y=Disease)) +
  geom_line(size=2,alpha=.86) + 
  theme_classic() + 
  xlab("Experienced temperature") + ylab(expression(R["0"]))+
  theme( legend.key.size = unit(.5, "cm"),
         legend.title =element_text(size=12,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=12,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=0, color = "black"),
         axis.text.y  = element_text(vjust=0.5, size=0, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         strip.text.x = element_text(size=20) )
con3.a/con3.a


ggsave("Concept_d.pdf", height=7,width = 5, dpi=320)

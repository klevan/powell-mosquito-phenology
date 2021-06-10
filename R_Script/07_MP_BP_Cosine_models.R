####### Powell Center: Phenological patterns of mosquitoes #######

# Travis McDevitt-Galles
# 04/26/2021
# title: 07_MP_BP_Cosine_models

# The goal of the following script is to explore the relationship between
# mosquito abundances and various climate factors

# load/ install required libraries for prism data acquisition and clean up

#Set working directory
setwd("C:/Users/tmcdevitt-galles/powell-mosquito-phenology")

#setwd("~/Desktop/Current_Projects/powell-mosquito-phenology")

library( dplyr )
library( tidyr )
library( ggplot2 )

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
cont.df <- contigus.df %>% select( -c("Lat", "Long", "Date"))

# Joining datasets so that prism data is now linked to count data
full.df <- left_join(complete.df, cont.df, by=c("Plot","Year","DOY"))

## Number of rows should match complete.df of 620662
dim(full.df) # 620662 X 33

############ subsetting to the single species, site and year #########

# We are only interested in Aedes vexans from WOOD site
toy.df <- filter(full.df, SciName== "Aedes vexans" & Site == "WOOD")

unique(toy.df$Year)

## lets first just estimate for one year, 2017 was a good year for me so 

toy.year.df <-filter(toy.df, Year == 2017)

## summarize to DOY level
toy.year.df <- toy.year.df %>% #filter(Plot == "WOOD_039") %>% 
  group_by(DOY,Plot) %>% 
  summarise(Count = sum(Count),
            TrapHours= sum(TrapHours)) %>% ungroup()
  
## Plot data
toy.year.df %>% 
ggplot( aes(x = DOY, y=Count))+geom_point()

## Rounding for easy density estimates

toy.year.df$MosDen <- as.integer(round(toy.year.df$Count/toy.year.df$TrapHours,0))

#### building functions for the birth pulse model ####

# Estimate the number of emerging mosquitoes across the year

# Parameters
# k = scaling factor proportional to the annual per capita birth rate
# t = the time steps will be the DOY
# s = birthing synchrony, how wide and tall the birthing peak is
# phi = the number of birth pulse events 
# omega = is time between birth pulse? 

bp <-function(k ,s, phi, omega,tmin, tmax){
  # vector to store my predicted abundance patterns
  season <- tmax - tmin
  pred_bp <- rep(NA, (season) )
 
  # Looping through the time steps to predict abundance
  for( i in 1:season){
    time <- i/season
    pred_bp[i] <- k * sqrt(s/pi) * exp( -s * cos( (pi*time) * (phi) - omega )^2 )
  }
  # returning the vector of predictions
  return( pred_bp )
}



at <- bp(k =6.24 ,s =8.01, phi =2.5, omega = 3 ,tmin = min(toy.year.df$DOY),
         tmax= max(toy.year.df$DOY))

plot(x=toy.year.df$DOY, y= toy.year.df$MosDen)
points(y=at, x=89:298,type="l")

#### Cosine function 

# Key parameters
# phi = number of birthing events
# k = scaler for max population size

cosinePop <-function(k, phi,tmin , tmax){
  # vector to store my predicted abundance patterns
  season <- tmax -tmin
  pred_cosine <- rep(NA, season)
  
  # Looping through the time steps to predict abundance
  for( i in 1:season){
    time <- i/season
    pred_cosine[i] <- k* cos( (pi*time) * (phi) )^2 
  }
  # returning the vector of predictions
  return( pred_cosine )
}


at <-  cosinePop(k=100,phi =.9 , tmin= 88, tmax=298)


plot(y=at, x= 89:298)

## Writing the likelihood model to use to optimize the parameter estimation

## negative log likelihood for birth pulse model

nll_BP <- function(par, n , tmin, tmax){
  pred <- bp( k= par[1], s= par[2], phi=par[3], omega= par[4], tmin= tmin, tmax=tmax)
  nll <- sum(- dpois(n, lambda = mean(pred), log=T))
  return(nll)
}

nll_Cosine <- function(par, n , tmin, tmax){
  pred <- cosinePop( k= par[1], s= par[2], phi=par[3], tmin= tmin, tmax=tmax)
  nll <- sum(- dpois(n, lambda = mean(pred), log=T))
  return(nll)
}


# Optimizing the GP model


start <- c(6, 10,1)

optim_BP <- optim( start, nll_BP,  n = toy.year.df$MosDen,
                   tmin = min(toy.year.df$DOY), tmax = max(toy.year.df$DOY))
optim_BP


# Calculating AIC 

nll <- optim_BP$value # likelihood
k <- 3 # number of parameters estimated
AIC_BP <- 2*nll+2*k

# Calculating weighted AIC
wAIC_BP <- AIC_BP + ( ( 2*k * (k=+1) ) / (length(toy.year.df$MosDen)-k-1))

AIC_BP # 1918.695
wAIC_BP # 1918.715  # no difference due to high sample size 

## Testing fitted parameters


fit <- bp(k =7.66 ,s =9.08, phi = 1 , tmin= 74, tmax=319)



plot(x=toy.year.df$DOY, y= toy.year.df$MosDen)
points(y=fit, x=75:319,type="l")

fit.df <- data.frame( fit= as.numeric(fit), DOY = as.integer(75:319))

ggplot( toy.year.df,aes(x=DOY, y=MosDen)) + geom_point(size=2,alpha=.7)+
  geom_line(data=fit.df,aes(x=DOY,y=fit),size=2,color='blue') + 
  theme_classic() + ylab("Mosquito density")+
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

  nll_Cosine <- function(par, n , tmin, tmax){
  pred <- cosinePop( k= par[1], phi=par[2], tmin= tmin, tmax=tmax)
  nll <- -sum(dpois(n, lambda = mean(pred), log=T))
  return(nll)
}


# selecting the starting points 
start <- c(0.5,1)

optim_cosine <- optim( start, nll_Cosine,  n = toy.year.df$MosDen,
                   tmin = min(toy.year.df$DOY), tmax = max(toy.year.df$DOY))
optim_cosine


fit <-  cosinePop(k=5.26,phi =1.87 , tmin= 74, tmax=319)

plot(x=toy.year.df$DOY, y= toy.year.df$MosDen)
points(y=fit, x=75:319,type="l")

fit.df <- data.frame( fit= as.numeric(fit), DOY = as.integer(75:319))

ggplot( toy.year.df,aes(x=DOY, y=MosDen)) + geom_point(size=2,alpha=.7)+
  geom_line(data=fit.df,aes(x=DOY,y=fit),size=2,color='blue') + 
  theme_classic() + ylab("Mosquito density")+
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


nll <- optim_BP$value # likelihood
k <-  2# number of parameters estimated
AIC_cosine <- 2*nll+2*k

# Calculating weighted AIC
wAIC_cosine <- AIC_cosine + ( ( 2*k * (k+1) ) / (length(toy.year.df$MosDen)-k-1))

AIC_cosine  # 1916.695
wAIC_cosine #  1916.708 # no difference due to high sample size 


### Cosine model has a slightly lower AIC score compared to birth pulse model
## delta AIC =~ 2 


######## Exploring the likelihood profile for the BP function #####

## Initial model parameter and fit
fit <- bp(k =12 ,s =3.5, phi = 1.3 , omega = 1.95,
          tmin= 74, tmax=319)

plot(x=toy.year.df$DOY, y= toy.year.df$MosDen)
points(y=fit, x=75:319,type="l")

## Getting intial log likelihood
pars <- c(12,3.5,1.3,1.95)

nll_BP(par = pars,n = toy.year.df$MosDen, tmin=74,tmax=319) # 1000.817


### K parameter
k_profile.df <- data.frame( 
                            Par = as.character(rep('K', 100)),
                            Coef = as.numeric(seq(2,20, length.out = 100)))

k_profile.df$nll <- NA

for( i in 1:100){
  pars <- c(k_profile.df$Coef[i],3.5,1.3,1.95)
   nll <- nll_BP(par = pars,n = toy.year.df$MosDen, tmin=74,tmax=319) 
   k_profile.df$nll[i] <- nll

}

## Synchrony parameter

s_profile.df <- data.frame( 
  Par = as.character(rep('s', 100)),
  Coef = as.numeric(seq(1,100, length.out = 100)))

s_profile.df$nll <- NA

for( i in 1:100){
  pars <- c(12,s_profile.df$Coef[i],1.3,1.95)
  nll <- nll_BP(par = pars,n = toy.year.df$MosDen, tmin=74,tmax=319) 
  s_profile.df$nll[i] <- nll
  
}

##phi parameter

phi_profile.df <- data.frame( 
  Par = as.character(rep('phi', 100)),
  Coef = as.numeric(seq(0,5, length.out = 100)))

phi_profile.df$nll <- NA

for( i in 1:100){
  pars <- c(12,2.4,phi_profile.df$Coef[i],1.95)
  nll <- nll_BP(par = pars,n = toy.year.df$MosDen, tmin=74,tmax=319) 
  phi_profile.df$nll[i] <- nll
  
}

## omega parameter

om_profile.df <- data.frame( 
  Par = as.character(rep('omega', 100)),
  Coef = as.numeric(seq(0,5, length.out = 100)))

om_profile.df$nll <- NA

for( i in 1:100){
  pars <- c(12,2.4,1.3,om_profile.df$Coef[i])
  nll <- nll_BP(par = pars,n = toy.year.df$MosDen, tmin=74,tmax=319) 
  om_profile.df$nll[i] <- nll
  
}

nll_profile <- rbind.data.frame(k_profile.df,s_profile.df,
                                phi_profile.df,om_profile.df)


ggplot(nll_profile,aes(x=Coef,y=nll,color=Par))+
  geom_line(size=2, alpha=.5)+ facet_wrap(~Par, scales="free") + 
  theme_classic()+ xlab( "Parameter value") +
  ylab( "negative log likelihood")+
  theme( legend.key.size = unit(1, "cm"),
         legend.title =element_text(size=18,margin = margin(r = 40, unit = "pt")),
         legend.text=element_text(size=18,margin = margin(r = 40, unit = "pt")), 
         legend.position = "none",
         axis.line.x = element_line(color="black") ,
         axis.ticks.y = element_line(color="black"),
         axis.ticks.x = element_line(color="black"),
         axis.title.x = element_text(size = rel(1.8)),
         axis.text.x  = element_text(vjust=0.5, size=16, color = "black"),
         axis.title.y = element_text(size = rel(1.8), angle = 90) ,
         axis.text.y=element_text(vjust=0.5, size=16),
         strip.text.x = element_text(size=20) )


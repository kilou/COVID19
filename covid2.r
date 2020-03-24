########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")

# Load data and parameters
# Data available on https://www.vd.ch/toutes-les-actualites/hotline-et-informations-sur-le-coronavirus/
library(readxl)
data <- as.data.frame(read_xlsx("data.xlsx"))
pars <- as.data.frame(read_xlsx("params.xlsx"))
data$date <- conv(data$date)
pars$date <- conv(pars$date)
today <- data$date[nrow(data)]

# ------------------------------------------------------------------------------------------------------
# VISUALIZE PRIOR DISTRIBUTIONS                        
level <- 0.95
alpha <- 1-level
p <- c(alpha/2,0.5,1-alpha/2)

# Growth parameter (restricted >=1)
mlam <- 1.25  # median growth
vlam <- 0.01  # variability
lam <- rlam(1e06,mlam,vlam)
plot(density(lam),xlab="Valeur",ylab="Densité",main="Paramètre de croissance exponentielle")
qlam <- quantile(lam,probs=p)
abline(v=qlam,lty=2)

# Proportion of incident cases that will require IC
mpic <- 0.15  # median proportion
vpic <- 0.05  # variability
pic <- rpic(1e06,mpic,vpic)
plot(density(pic),xlab="Proportion",ylab="Densité",main="Proportion de cas incidents nécessitant des soins intensifs")
qpic <- quantile(pic,probs=p)
abline(v=qpic,lty=2)

# Lag between COVID-19 test and ICU admission
mlag <- 8  # mean lag
vlag <- 15 # variability. vlag must be >=mlag with vlag=mlag corresponding to Poisson model
lag <- rlag(1e06,mlag,vlag)
plot(density(lag,bw=1),xlab="Jours",ylab="Densité",main="Intervalle entre dépistage et entrée aux soins intensifs")
qlag <- quantile(lag,probs=p)
abline(v=qlag,lty=2)

# ICU length of stay
mlos <- 9   # mean LOS
vlos <- 18  # variability. vlos must be >=mlos with vlos=mlos corresponding to Poisson model 
los <- rlos(1e06,mlos,vlos)
plot(density(los,bw=1),xlab="Jours",ylab="Densité",main="Durée de séjour aux soins intensifs")
qlos <- quantile(los,probs=p)
abline(v=qlos,lty=2)

# ------------------------------------------------------------------------------------------------------
# FORECASTS USING PRED.COVID() FUNCTION                        

# Forecasts ICU beds requirements
pred <- pred.covid(nday=7,nsim=2000,pars,data,ncpu=4)
days <- as.Date(strptime(colnames(pred$nbed),format="%d.%m.%Y"))

# Plot cumulative counts
qntot <- t(apply(pred$ntot,2,quantile,probs=p))
plot(range(days),range(qntot),type="n",xlab="",ylab="Nombre de cas")
title("Nombre cumulatif de cas confirmés COVID-19 dans le canton de Vaud")
polygon(x=c(days,rev(days)),y=c(qntot[,1],rev(qntot[,3])),col="grey",border=NA)
lines(days,qntot[,2],lwd=2)
points(data$date,data$ntot,pch=19)
abline(v=today,lty=2)

# Plot nb ICU beds required
qnbed <- t(apply(pred$nbed,2,quantile,probs=p))
plot(range(days),range(qnbed),type="n",xlab="",ylab="Nombre de lits")
title("Nombre de lits occupés aux soins intensifs dans le canton de Vaud")
polygon(x=c(days,rev(days)),y=c(qnbed[,1],rev(qnbed[,3])),col="grey",border=NA)
lines(days,qnbed[,2],lwd=2)
points(data$date,data$nicu,pch=19)
abline(v=today,lty=2)

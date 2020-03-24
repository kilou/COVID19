########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")
source("functions_test.r")

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
histo(lam,p)

# Proportion of incident cases that will require IC
mpic <- 0.15  # median proportion
vpic <- 0.05  # variability
pic <- rpic(1e06,mpic,vpic)
histo(pic,p)

# Lag between COVID-19 test and ICU admission
mlag <- 8  # mean lag
vlag <- 15 # variability. vlag must be >=mlag with vlag=mlag corresponding to Poisson model
lag <- rlag(1e06,mlag,vlag)
histo(lag,p)

# ICU length of stay
mlos <- 9   # mean LOS
vlos <- 18  # variability. vlos must be >=mlos with vlos=mlos corresponding to Poisson model 
los <- rlos(1e06,mlos,vlos)
histo(los,p)

# ------------------------------------------------------------------------------------------------------
# FORECASTS USING PRED.COVID() FUNCTION                        

# Forecasts ICU beds requirements
pred <- pred.covid(nday=7,nsim=2000,pars,data,ncpu=4)

# Plot cumulative counts
plot.covid(pred,what="ntot",prob=p)

# Plot nb ICU beds required
plot.covid(pred,what="nbed",prob=p)



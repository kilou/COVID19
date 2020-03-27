########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")
#shiny::runApp("app.R")

# Load data and parameters
library(readxl)
#data <- as.data.frame(read_xlsx("data.xlsx"))
#data$date <- conv(data$date)
data <- import.covid(
  input.file="20.03.26 - Données hop COVID - anonymisées.xlsx",
  input.sheet="données",
  output.file=NULL,
  start.date="25.02.2020"
)
pars <- as.data.frame(read_xlsx("params.xlsx"))
pars$date <- conv(pars$date)
today <- data$date[nrow(data)]

# ------------------------------------------------------------------------------------------------------
# VISUALIZE PRIOR DISTRIBUTIONS                        
level <- 0.90
alpha <- 1-level
p <- c(alpha/2,0.5,1-alpha/2)

# Growth parameter for nb of hospitalized patients (restricted >=1)
mlam <- 1.20  # median growth
vlam <- 0.05  # variability
lam <- rlam(1e06,mlam,vlam)
histo(lam,p)

# Proportion of hospitalized patients that will require IC
mpic <- 0.28  # median proportion
vpic <- 0.10  # variability
pic <- rpic(1e06,mpic,vpic)
histo(pic,p)

# Lag between hospitalization and ICU admission (only for patients that will require in ICU!)
mlag <- 3  # mean lag
vlag <- 5  # variability. vlag must be >=mlag with vlag=mlag corresponding to Poisson model
lag <- rlag(1e06,mlag,vlag)
histo(lag,p)

# ICU length of stay
mlos <- 8   # mean LOS
vlos <- 50  # variability. vlos must be >=mlos with vlos=mlos corresponding to Poisson model 
los <- rlos(1e06,mlos,vlos)
histo(los,p)

# ------------------------------------------------------------------------------------------------------
# FORECASTS USING PRED.COVID() FUNCTION                        

# Forecasts ICU beds requirements
pred <- pred.covid(nday=60,nsim=2000,pars,data,ncpu=4)

# Plot cumulative counts
plot.covid(pred,what="nhos",prob=p)

# Plot nb ICU beds required
plot.covid(pred,what="nbed",prob=p)



########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")
#shiny::runApp("app.R")

# Load data and parameters
data <- import.covid(
  input.file="data/20.03.30 - Données hop COVID - anonymisées.xlsx",
  start.date="2020-02-25",
  date.format="%Y-%m-%d"
)
pars <- as.data.frame(readxl::read_xlsx("params.xlsx"))
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
mpic <- 0.2  # median proportion
vpic <- 0.10  # variability
pic <- rpic(1e06,mpic,vpic)
histo(pic,p)

# Lag between hospitalization and ICU admission (only for patients that will require in ICU!)
mlag <- 3  # mean lag
vlag <- 5  # variability. vlag must be >=mlag with vlag=mlag corresponding to Poisson model
lag <- rlag(1e06,mlag,vlag)
histo(lag,p)

# ICU length of stay
mlos <- 13   # mean LOS
vlos <- 154  # variability. vlos must be >=mlos with vlos=mlos corresponding to Poisson model 
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

# ------------------------------------------------------------------------------------------------------
# ESTIMATE LAG AND LOS DISTRIBUTIONS ON INDIVIDUAL PATIENT DATA
ipd <- import.ipd(
  input.file="data/20.03.30 - Données hop COVID - anonymisées.xlsx",
  input.sheet="Backlog",
  date.format="%Y-%m-%d"
)
today <- max(ipd$hos_in,na.rm=T)

# Negative binomial distribution for lag
fit.nb(ipd$icu_lag)

# negative binomial distribution for LOS
los <- ipd$icu_los
cens <- (!is.na(ipd$icu_in) & is.na(ipd$icu_out))*1
los[cens==1] <- today-ipd$icu_in[cens==1]
fit.nb(los,cens)

# ------------------------------------------------------------------------------------------------------
# ESTIMATE MEAN AND VARIANCE OF EXPONENTIAL GROWTH PARAMETER ON LAST 15 DAYS
plot(log(nhos)~date,data=data)
nhos <- subset(data,date>=today-15)$nhos
lam <- nhos[-1]/nhos[-length(nhos)]
mean(lam)
sd(lam)

########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")
#shiny::runApp("app.R")

# Load data and parameters
data <- import.covid(
  input.file="data/20.04.14 - Données REDCap hôpitaux anonymisés.xlsx",
  #input.file="data/data_09042020.xlsx",
  start.date="02/25/2020",
  end.date="04/13/2020",
  date.format="%m/%d/%Y"
)
pars <- as.data.frame(readxl::read_xlsx("params.xlsx"))
pars$date <- conv(pars$date)
today <- data$date[nrow(data)]

# Load parameters for survival models
pars_surv <- as.data.frame(readxl::read_xlsx("params_surv.xlsx"))

# ------------------------------------------------------------------------------------------------------
# VISUALIZE PRIOR DISTRIBUTIONS                        
level <- 0.90
alpha <- 1-level
p <- c(alpha/2,0.5,1-alpha/2)

# Growth parameter for nb of hospitalized patients (restricted >=1)
megp <- 1.12  # median growth
vegp <- 0.08  # variability
egp <- regp(1e06,megp,vegp)
histo(egp,p)

# Proportion of hospitalized patients that will require IC at some point
micp <- 0.2  # median proportion
vicp <- 0.10 # variability
icp <- ricp(1e06,micp,vicp)
histo(icp,p)

# Proportion of patients that will require IC at some point and who will be admitted in ICU at the end of their lag
madp <- 1    # median proportion (should be 1 in absence of restriction)
vadp <- 0.10 # variability
adp <- radp(1e06,madp,vadp)
histo(adp,p)

# Lag between hospitalization and ICU admission (only for patients that will require in ICU!)
mlag <- 2  # mean lag
vlag <- 9  # variability. vlag must be >=mlag with vlag=mlag corresponding to Poisson model
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
pred <- pred.covid(nday=60,nsim=2000,pars,pars_surv,data,ncpu=4)

# Plot cumulative counts
plot.covid(pred,what="nhos",prob=p)

# Plot nb ICU beds required
plot.covid(pred,what="nbed",prob=p)

# Plot nb of daily deaths
plot.covid(pred,what="ndead_daily",prob=p)

# Plot cumulative nb of deaths
plot.covid(pred,what="ndead_cumul",prob=p)

# ------------------------------------------------------------------------------------------------------
# ESTIMATE LAG AND LOS DISTRIBUTIONS ON INDIVIDUAL PATIENT DATA
ipd <- import.ipd(
  input.file="data/20.04.14 - Données REDCap hôpitaux anonymisés.xlsx",
  input.sheet=1,
  date.format="%m/%d/%Y"
)
today <- max(ipd$hos_in,na.rm=T)

# Negative binomial distribution for lag
fit.nb(ipd$icu_lag)

# Negative binomial distribution for LOS
los <- ipd$icu_los
cens <- (!is.na(ipd$icu_in) & is.na(ipd$icu_out))*1
los[cens==1] <- today-ipd$icu_in[cens==1]
fit.nb(los,cens)

# ------------------------------------------------------------------------------------------------------
# ESTIMATE MEAN AND VARIANCE OF EXPONENTIAL GROWTH PARAMETER ON LAST 15 DAYS
plot(log(nhos)~date,data=data)
nhos <- subset(data,date>=today-15)$nhos
egp <- nhos[-1]/nhos[-length(nhos)]
mean(egp)
sd(egp)

egp <- data$nhos[-1]/data$nhos[-nrow(data)]
plot(data$date[-1],egp)


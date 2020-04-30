########################################################################################################
# COVID-19 FORECASTS                                                                                   #
########################################################################################################
setwd("~/Documents/Work/COVID19")
source("functions.r")
#shiny::runApp("app.R")

# Load data and parameters
data <- import.covid(
  input.file="data/20.04.17 - Données REDCap hôpitaux anonymisés.xlsx",
  #input.file="data/data_14042020.xlsx",
  start.date=NA,
  end.date="04/13/2020",
  date.format="%m/%d/%Y"
)
#writexl::write_xlsx(data,path="data/data_14042020.xlsx")
today <- data$date[nrow(data)]

# Load parameters
pars <- pars.covid(
  input.file="params.xlsx",
  date.format="%d.%m.%Y"
)

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

# Plot age distribution for each sex
page <- as.data.frame(readxl::read_xlsx("params.xlsx",sheet="age_distrib"))[1,-1]
pfem <- as.data.frame(readxl::read_xlsx("params.xlsx",sheet="sex_distrib"))[1,-1]
age.breaks <- seq(0,105,by=15)
pop <- rpop(1e06,age.breaks,page,pfem)

xM <- pop$age[pop$sex=="M"]; attr(xM,"breaks") <- age.breaks
xF <- pop$age[pop$sex=="F"]; attr(xF,"breaks") <- age.breaks
par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
histo(xM); title("Males")
histo(xF); title("Females")

# ------------------------------------------------------------------------------------------------------
# FORECASTS USING PRED.COVID() FUNCTION                        

# Forecasts ICU beds requirements
pred <- pred.covid(nday=60,nsim=2000,pars,pars_surv,data,type=NULL,ncpu=4)

# Plot cumulative counts
plot.covid(pred,what="nhos",prob=p,from="02/25/2020",date.format="%m/%d/%Y")

# Plot nb ICU beds required
plot.covid(pred,what="nbed",prob=p,from="02/25/2020",date.format="%m/%d/%Y")

# Plot nb of daily deaths
plot.covid(pred,what="ndead_daily",prob=p,from="02/25/2020",date.format="%m/%d/%Y")

# Plot cumulative nb of deaths
plot.covid(pred,what="ndead_cumul",prob=p,from="02/25/2020",date.format="%m/%d/%Y")

# ------------------------------------------------------------------------------------------------------
# ESTIMATE LAG AND LOS DISTRIBUTIONS ON INDIVIDUAL PATIENT DATA
data <- import.covid(
  input.file="data/20.04.14 - Données REDCap hôpitaux anonymisés.xlsx",
  start.date=NA,
  end.date="04/13/2020",
  date.format="%m/%d/%Y"
)
ipd <- attr(data,"ipd")
today <- max(ipd$hos_in,na.rm=T)

lag <- as.numeric(ipd$icu_in-ipd$hos_in)

# Negative binomial distribution for lag
fit.nb(lag)

# Negative binomial distribution for ICU LOS
los <- as.numeric(ipd$icu_out-ipd$icu_in)
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


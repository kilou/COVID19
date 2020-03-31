########################################################################################################
# NECESSARY FUNCTIONS FOR COVID-19 FORECASTS                                                           #
########################################################################################################
# ------------------------------------------------------------------------------------------------------
# Convert dates
conv <- function(x, format = "%d.%m.%Y") {
  if (any(class(x) %in% c("POSIXct", "POSIXt"))) {
    x <- as.Date(x)
  } else if (any(class(x) %in% "character")) {
    x <- as.Date(strptime(as.character(x), format = format))
  } else if (!any(class(x) %in% "Date")) {
    stop("Wrong date format")
  }
  return(x)
}

# ------------------------------------------------------------------------------------------------------
# Logit and expit functions
logit <- function(x){log(x/(1-x))}
expit <- function(x){exp(x)/(1+exp(x))}

# ------------------------------------------------------------------------------------------------------
# Random sample for exponential growth parameter (constrained >1)
rlam <- function(
  n,    # nb draws
  mlam, # median of growth parameter (typically 1.25)
  vlam  # variability
){1+exp(rnorm(n,log(mlam-1),sqrt(vlam)))}

# ------------------------------------------------------------------------------------------------------
# Random sample for proportion of hospitalized patients requiring intensive cares
rpic <- function(
  n,    # nb draws
  mpic, # median proportion
  vpic  # variability
){expit(rnorm(n,logit(mpic),sqrt(vpic)))}

# ------------------------------------------------------------------------------------------------------
# Random sample for lag (between hospitalization and ICU admission)
rlag <- function(
  n,    # nb draws
  mlag, # expected value for lag (in days)
  vlag  # variability
){rnbinom(n,mu=mlag,size=mlag^2/(vlag-mlag))}

# ------------------------------------------------------------------------------------------------------
# Random sample for ICU length of stay (LOS)
rlos <- function(
  n,    # nb draws
  mlos, # expected value for length of stay (in days)
  vlos  # variability
){rnbinom(n,mu=mlos,size=mlos^2/(vlos-mlos))}

# ------------------------------------------------------------------------------------------------------
# Random sample for proportion of patients requiering IC at the end of their lag and who will be admitted in ICU
radm <- function(
  n,    # nb draws
  madm, # median proportion
  vadm  # variability
){expit(rnorm(n,logit(madm),sqrt(vadm)))}

# ------------------------------------------------------------------------------------------------------
# Define RGB code for shiny blue (to be used in rgb() function possibly with alpha transparency)
rgb.blue <- t(col2rgb("#428bca"))/255

# ------------------------------------------------------------------------------------------------------
# Histogram for parameters
histo <- function(x,prob){
  h <- hist(x,breaks=30,xlab="",ylab="",main="",col=rgb(rgb.blue,alpha=0.5),yaxt="n",yaxs="i",freq=FALSE)
  if(!is.null(prob)){
    stopifnot(length(prob)==3)
    q <- quantile(x,probs=prob)
    abline(v=q[c(1,3)],lty=2,lwd=2,col="red")
    abline(v=q[2],lwd=2,col="red")
    mtext(paste0(100*prob,"%"),side=3,at=q,line=c(0,0.7,0),col="red",cex=0.8)
  }
}

# ------------------------------------------------------------------------------------------------------
# Plot function for object returned by pred.covid()
plot.covid <- function(
  object.covid,           # object returned by pred.covid()
  what="nbed",            # name of element to plot in object.covid
  prob=c(0.025,0.5,0.975) # quantiles to plot (length=3!)
){
  stopifnot(length(prob)==3)
  data <- object.covid$data
  X <- object.covid[[what]]
  Q <- t(apply(X,2,quantile,probs=prob))
  days <- as.Date(strptime(colnames(X),format="%d.%m.%Y"))
  today <- data$date[nrow(data)]
  past <- which(days<=today)
  futur <- which(days>=today)
  
  if(what=="nbed"){
    tit <- "Number of occupied beds in ICUs"
    ylb <- "Number of beds"
    y <- data$nicu
  }
  if(what=="nhos"){
    tit <- "Cumulative counts of hospitalized patients"
    ylb <- "Counts"
    y <- data$nhos
  }

  par(mar=c(3,5,4,3),mgp=c(1.8,0.6,0))
  plot(range(days),range(c(y,Q)),type="n",xlab="",ylab="",las=1)
  ytck1 <- pretty(c(y,Q))
  ytck2 <- seq(min(ytck1),max(ytck1),by=diff(ytck1)[1]/5)
  abline(h=ytck1,col="grey",lwd=0.5)
  abline(h=ytck2,col="grey",lwd=0.5,lty=2)
  abline(v=days,col="grey",lwd=0.5)
  mtext(ylb,side=2,line=2.8)
  title(tit)
  polygon(x=c(days[past],rev(days[past])),y=c(Q[past,1],rev(Q[past,3])),col="lightgrey",border=NA)
  lines(days[past],Q[past,2],lwd=2,col="darkgrey")
  polygon(x=c(days[futur],rev(days[futur])),y=c(Q[futur,1],rev(Q[futur,3])),col=rgb(rgb.blue,alpha=0.5),border=NA)
  lines(days[futur],Q[futur,2],lwd=2,col="red")
  points(data$date,y,pch=19,col="black")
  points(days[futur[-1]],Q[futur[-1],2],pch=19,col="red")
  abline(v=today,lty=2)
  mtext(today,side=3,at=today,line=0.2)
  abline(h=0)
  legend("topleft",legend=c("Observed counts","Predicted counts"),pch=c(19,19),col=c("black","red"),bty="n",cex=1)
  mtext(paste0(100*prob,"%"),side=4,at=Q[nrow(Q),],cex=0.8,las=1,col=c(rgb(rgb.blue),"red",rgb(rgb.blue)),line=0.25)
}

# ------------------------------------------------------------------------------------------------------
# Import individual patient data
import.ipd <- function(
  input.file,              # xlsx input data file with individual patient data
  input.sheet,             # sheet in input.file where data are located
  date.format="%d.%m.%Y"
){
  # Read individual patient data
  raw <- as.data.frame(readxl::read_xlsx(input.file,sheet=input.sheet))
  id <- raw[,"No interne*"]
  age <- raw[,"age"]; age[age==0] <- NA
  sex <- factor(raw[,"sex"],levels=c("M","F"))
  hos_in <- conv(format(as.Date(raw[,"arrivee_hopital"]),"%d.%m.%Y"))
  icu_in <- conv(format(as.Date(raw[,"debut_soins_intensifs"]),"%d.%m.%Y"))
  icu_out <- conv(format(as.Date(raw[,"fin_soins_intensifs"]),"%d.%m.%Y"))
  hos_out <- conv(format(as.Date(raw[,"sortie_hopital"]),"%d.%m.%Y"))
  dead <- raw[,"deces"]
  
  # Calculate lag and ICU length of stay
  icu_lag <- icu_in-hos_in
  icu_los <- icu_out-icu_in
  hos_los <- hos_out-hos_in
  
  # Return IPD data
  data <- cbind.data.frame(id,age,sex,hos_in,icu_in,icu_out,hos_out,dead,icu_lag,icu_los,hos_los)
  data
}

# ------------------------------------------------------------------------------------------------------
# Import data. Input file can be either a file with date, nhos and nicu columns or a file with individual patient data
import.covid <- function(
  input.file="data.xlsx", # xlsx input data file
  start.date=NA,  # return counts only from this date onwards (but counts are cumulated from the start of input.file)
  date.format="%d.%m.%Y"
){

  # Detect file type (individual patient data or counts)
  sheets <- readxl::excel_sheets(input.file)
  nsheets <- length(sheets)
  for(k in 1:nsheets){
    raw <- as.data.frame(readxl::read_xlsx(input.file,sheet=k))
    if(colnames(raw)[1]=="No interne*"){type <- "ipd"; sheet <- k; break}
    if(sum(colnames(raw)=="nhos")>0){type <- "counts"; sheet <- k; break}
  }
  
  if(type=="ipd"){
    # Individual patient data
    id <- raw[,"No interne*"]
    hos_in <- conv(raw[,"arrivee_hopital"],date.format)
    icu_in <- conv(raw[,"debut_soins_intensifs"],date.format)
    icu_out <- conv(raw[,"fin_soins_intensifs"],date.format)
    
    # Calculate daily cumulative count of hospitalized patients and daily nb of patients in ICU
    days <- min(hos_in,na.rm=T)+c(0:diff(range(hos_in,na.rm=T)))
    ndays <- length(days)
    nhos <- nicu <- numeric(ndays)
    for(j in 1:ndays){
      nhos[j] <- sum(hos_in<=days[j],na.rm=T)
      nicu[j] <- sum(icu_in<=days[j] & is.na(icu_out),na.rm=T) + sum(icu_in<=days[j] & icu_out>=days[j],na.rm=T)
    }
    data <- data.frame(date=days,nhos=nhos,nicu=nicu)
  } else {
    # Data with nhos and nicu
    data <- raw
    data$date <- conv(data$date, date.format)
  }
  if (!is.na(start.date)) {
    start.date <- conv(start.date, format = date.format)
    data <- subset(data,date>=start.date)
  }
  data$nhos <- as.integer(data$nhos)
  data$nicu <- as.integer(data$nicu)
  data
}

# ------------------------------------------------------------------------------------------------------
# Forecast nb of ICU beds
pred.covid <- function(
  nday,     # nb of days to forecast
  nsim,     # nb of simulations
  pars,     # dataframe with parameters
  data,     # dataframe with VD data
  ncpu,     # nb of parallel processes (use 1 for serial compiutations)
  seed=1234 # seed for reproducible computations
){
  set.seed(seed)
  
  # Check consistency of dates
  if(min(pars$date)>min(data$date)){stop("First date defining parameters must be anterior or equal to first date in the data!")}
  
  # Some useful things
  today <- data$date[nrow(data)]       # today i.e. last date entered in data
  days <- c(data$date,today+c(1:nday)) # vector of days for observed data and predictions
  j <- which(days==today)              # index of today
  
  # Get parameters for each day in "days"
  ind <- sapply(days,function(d){max(which(pars$date<=d))})
  mlam <- pars$mlam[ind]
  vlam <- pars$vlam[ind]
  mpic <- pars$mpic[ind]
  vpic <- pars$vpic[ind]
  mlag <- pars$mlag[ind]
  vlag <- pars$vlag[ind]
  mlos <- pars$mlos[ind]
  vlos <- pars$vlos[ind]
  #pac <- pars$pac[ind]

  # Fill-in observed cumulative count of hospitalized patients
  nhos <- matrix(nrow=nsim,ncol=j+nday)
  nhos[,1:j] <- t(data$nhos)[rep(1,nsim),]
  
  # Predict future cumulative counts of hospitalized patients using exponential growth parameter
  for(k in (j+1):(j+nday)){nhos[,k] <- round(nhos[,k-1]*rlam(nsim,mlam[k],vlam[k]))}
  
  # Calculate daily new hospitalizations (i.e. incident counts)
  ninc <- matrix(nrow=nsim,ncol=j+nday)
  ninc[,1] <- nhos[,1]
  for(k in 1:(j+nday-1)){ninc[,k+1] <- nhos[,k+1]-nhos[,k]}
  if(sum(ninc<0)>0){stop("Some incident counts are negative!")}
  
  # Calculate nb of ICU beds required (function to be parallelized)
  # ------------------------------------------------------------------------------------------------------
  fun_pic_on_hos_entry <- function(s){
    # Simulate nb of patient hospitalized on day j that will require IC at some point
    pic <- unlist(mapply(rpic,n=1,mpic=mpic,vpic=vpic,SIMPLIFY=FALSE))
    npat <- round(pic*ninc[s,])
    
    hos.in <- unlist(mapply(rep,x=1:(j+nday),times=npat,SIMPLIFY=FALSE))     # define hospitalization day (before ICU) for these patients
    lag <- unlist(mapply(rlag,n=npat,mlag=mlag,vlag=vlag,SIMPLIFY=FALSE))    # ICU lag for these patients
    los <- unlist(mapply(rlos,n=npat,mlos=mlos,vlos=vlos,SIMPLIFY=FALSE))    # length of stay in ICU for these patients
    
    # Define ICU day-in and day-out for these patients
    icu.in <- hos.in+lag
    icu.out <- icu.in+los
    
    # Fill-in bed occupancy matrix in ICU
    occ <- matrix(0,nrow=sum(npat),ncol=j+nday)
    for(i in 1:nrow(occ)){occ[i,which(c(1:(j+nday))%in%c(icu.in[i]:icu.out[i]))] <- 1}
    
    # Return daily nb of occupied ICU beds
    apply(occ,2,sum)
  }
  
  # ------------------------------------------------------------------------------------------------------
  fun_pic_on_icu_entry <- function(s){
    npat <- ninc[s,] # nb of new hospitalized patients on each day (no matter if they will need IC or not)
    hos.in <- unlist(mapply(rep,x=1:(j+nday),times=npat,SIMPLIFY=FALSE))     # define hospitalization day (before ICU) for these patients
    lag <- unlist(mapply(rlag,n=npat,mlag=mlag,vlag=vlag,SIMPLIFY=FALSE))    # ICU lag for these patients (assuming all hospitalized patients will require IC at some point...)
    los <- unlist(mapply(rlos,n=npat,mlos=mlos,vlos=vlos,SIMPLIFY=FALSE))    # length of stay in ICU for these patients
    
    # Define theoretical ICU day-in and day-out for all hospitalized patients
    icu.in <- hos.in+lag
    icu.out <- icu.in+los
    
    # Only accept a proportion "pic" of hospitalized patients in ICU on the day of their theoretical ICU admission
    pic <- unlist(mapply(rpic,n=1,mpic=mpic,vpic=vpic,SIMPLIFY=FALSE)) # simulated pic on each day
    for(k in 1:(j+nday)){
      sel <- which(icu.in==k); nsel <- length(sel)
      if(nsel>0){
        accept <- rbinom(nsel,size=1,prob=pic[k])
        icu.in[sel] <- icu.in[sel]*accept    # all patients with icu.in=0 are not accepted
        icu.out[sel] <- icu.out[sel]*accept  # all patients with icu.out=0 are not accepted
      }
    }
    
    # Restrict attention to patients that will be accepted in ICU
    icu.in <- icu.in[icu.in>0]
    icu.out <- icu.out[icu.out>0]
    
    # Fill-in bed occupancy matrix in ICU
    occ <- matrix(0,nrow=length(icu.in),ncol=j+nday)
    for(i in 1:nrow(occ)){occ[i,which(c(1:(j+nday))%in%c(icu.in[i]:icu.out[i]))] <- 1}
    
    # Return daily nb of occupied ICU beds
    apply(occ,2,sum)
  }
  
  # ------------------------------------------------------------------------------------------------------
  fun_pic_on_hos_entry_with_pac <- function(s){
    # Simulate nb of patient hospitalized on day j that will require IC at some point
    pic <- unlist(mapply(rpic,n=1,mpic=mpic,vpic=vpic,SIMPLIFY=FALSE))
    npat <- round(pic*ninc[s,])
    
    hos.in <- unlist(mapply(rep,x=1:(j+nday),times=npat,SIMPLIFY=FALSE))     # define hospitalization day (before ICU) for these patients
    lag <- unlist(mapply(rlag,n=npat,mlag=mlag,vlag=vlag,SIMPLIFY=FALSE))    # ICU lag for these patients
    los <- unlist(mapply(rlos,n=npat,mlos=mlos,vlos=vlos,SIMPLIFY=FALSE))    # length of stay in ICU for these patients
    
    # Define ICU day-in for these patients
    icu.in <- hos.in+lag
    
    # Only accept a proportion "pac" of patients in ICU on the day of their ideal ICU admission. Put non-accepted patient in a queue
    for(k in 1:(j+nday)){
      sel <- which(icu.in==k); nsel <- length(sel) # select patients that should theoretically be admitted to ICU on day k
      if(nsel>0){
        acc <- rbinom(nsel,size=1,prob=pac[k])
        ref <- sel[which(acc==0)]    # identify patients that will be refused on day k
        icu.in[ref] <- icu.in[ref]+1 # set these patients on queue and make them come back on day k+1
      }
    }
    
    # Define ICU day-out for these patients (LOS not affected by queue...)
    icu.out <- icu.in+los
    
    # Fill-in bed occupancy matrix in ICU
    occ <- matrix(0,nrow=sum(npat),ncol=j+nday)
    for(i in 1:nrow(occ)){occ[i,which(c(1:(j+nday))%in%c(icu.in[i]:icu.out[i]))] <- 1}
    
    # Return daily nb of occupied ICU beds
    apply(occ,2,sum)
  }
  
  # Run serial/parallel calculations
  fun <- fun_pic_on_icu_entry
  if(ncpu>1){
    require(snowfall)
    sfInit(parallel=TRUE, cpus=ncpu)
    sfExportAll() 
    nbed <- t(sfSapply(1:nsim, fun))
    sfStop()
  } else {
    nbed <- matrix(nrow=nsim,ncol=j+nday)
    for(s in 1:nsim){
      cat("Progress: ",round(100*s/nsim),"%\r",sep=""); flush.console()
      nbed[s,] <- fun(s)
    }
  }
  colnames(nhos) <- colnames(ninc) <- colnames(nbed) <- format(days,format="%d.%m.%Y")
  list(
    nhos=nhos, # cumulative counts of hospitalized patients
    ninc=ninc, # daily new hopitalized patients
    nbed=nbed, # predicted nb of occupied ICU beds
    data=data  # return data (for plotting)
  )
}

# ------------------------------------------------------------------------------------------------------
# Fit negative binomial distribution to a vector of counts x with possibly right-censored observations
# Uses a grid search on integer values
fit.nb <- function(
  x,           # count vector (e.g. nb of days)
  cens=NULL,   # binary indicator for right-censored (1) or observed (0) data. If NULL, x is fully observed
  mu.max=50,   # maximum mean value for grid search
  var.max=300  # maximum variance value for grid search
){
  x <- as.numeric(x)
  if(is.null(cens)){cens <- 0*x}
  
  # Remove missing values
  keep <- which(!is.na(x))
  x <- x[keep]
  cens <- cens[keep]
  
  # Define parameter grid
  grid <- expand.grid(m=c(1:mu.max),v=c(1:var.max))
  grid <- grid[which(grid$v>=grid$m),]
  
  # Define (censored) log-likelihood for negative binomial distribution
  nb.llik <- function(pars,x,cens){
    m <- pars[1]
    v <- pars[2]
    size <- m^2/(v-m)
    obs <- which(cens==0)
    cns <- which(cens==1)
    ll <- numeric(length(x))
    if(length(obs)>0){ll[obs] <- dnbinom(x[obs],size=size,mu=m,log=TRUE)}
    if(length(cns)>0){ll[cns] <- pnbinom(x[cns],size=size,mu=m,lower.tail=FALSE,log.p=TRUE)}
    sum(ll)
  }
  
  # Grid search
  ll <- as.numeric(apply(grid,1,nb.llik,x=x,cens=cens))
  best <- rev(order(ll))[1]
  
  list(mean=grid$m[best],variance=grid$v[best],loglik=ll[best])
}

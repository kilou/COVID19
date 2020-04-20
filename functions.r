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
regp <- function(
  n,    # nb draws
  megp, # median of growth parameter (typically 1.25)
  vegp  # variability
){1+exp(rnorm(n,log(megp-1),sqrt(vegp)))}

# ------------------------------------------------------------------------------------------------------
# Random sample for proportion of hospitalized patients that will require intensive cares at some point (defines disease severity)
ricp <- function(
  n,    # nb draws
  micp, # median proportion
  vicp  # variability
){expit(rnorm(n,logit(micp),sqrt(vicp)))}

# ------------------------------------------------------------------------------------------------------
# Random sample for proportion of patients requiring intensive cares that are effectively admitted in ICU at the end of their lag (defines admission policy in ICU)
radp <- function(
  n,    # nb draws
  madp, # median proportion
  vadp  # variability (vadp=0 if madp=0 or 1)
){if(madp==1 | madp==0){rep(madp,n)}else{expit(rnorm(n,logit(madp),sqrt(vadp)))}}

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

plos<- function(
  q,
  mlos,
  vlos
){pnbinom(q,mu=mlos,size=mlos^2/(vlos-mlos))}

qlos<- function(
  p,
  mlos,
  vlos
){qnbinom(p,mu=mlos,size=mlos^2/(vlos-mlos))}

# ------------------------------------------------------------------------------------------------------
# Draw age category
ragecat <- function(n,prob){
  ind <- t(rmultinom(n,size=1,prob=prob))
  if(is.null(colnames(ind))){colnames(ind) <- c(1:length(prob))}
  agecat <- integer(n)
  for(k in 1:ncol(ind)){
    pos <- which(ind[,k]==1)
    agecat[pos] <- rep(k,length(pos))
  }
  agecat
}

# ------------------------------------------------------------------------------------------------------
# Draw sex
rsex <- function(n,pfemale=0.4){
  sex <- rbinom(n,size=1,prob=pfemale)
  factor(sex,levels=c(0,1),labels=c("M","F"))  
}

# ------------------------------------------------------------------------------------------------------
# Define RGB code for shiny blue (to be used in rgb() function possibly with alpha transparency)
rgb.blue <- t(col2rgb("#428bca"))/255

# ------------------------------------------------------------------------------------------------------
# Histogram for parameters
histo <- function(x,prob){
  if(var(x)>0){
    h <- hist(x,breaks=30,xlab="",ylab="",main="",col=rgb(rgb.blue,alpha=0.5),yaxt="n",yaxs="i",freq=FALSE)
    if(!is.null(prob)){
      stopifnot(length(prob)==3)
      q <- quantile(x,probs=prob)
      abline(v=q[c(1,3)],lty=2,lwd=2,col="red")
      abline(v=q[2],lwd=2,col="red")
      mtext(paste0(100*prob,"%"),side=3,at=q,line=c(0,0.7,0),col="red",cex=0.8)
    }
  } else {
    plot(c(0,1),c(0,1),type="n",axes=FALSE,xlab="",ylab="",yaxs="i")
    polygon(c(0.48,0.52,0.52,0.48),c(0,0,1,1),col=rgb(rgb.blue,alpha=0.5))
    axis(side=1,at=0.5,labels=mean(x))
    abline(h=0)
  }
}

pred.h <- function(
  t,    # time (typically length of stay)
  agecat, 
  mu,
  sigma
){
  t[t==0] <- 0.001
  h <- numeric(length(t))
  for(k in 1:max(agecat)){
    pos <- which(agecat==k)
    h[pos] <- exp((log(t[pos])-mu[k])/sigma[k])/(sigma[k]*t[pos])
  }
  h
}

sim.los <- function(
  agecat, 
  mu,
  sigma,
  left=NULL # left censoring
){
  n <- length(agecat)
  if(is.null(left)){left <- rep(0,n)}
  los <- integer(n)
  for(k in 1:max(agecat)){
    pos <- which(agecat==k)
    pleft <- pweibull(left[pos],shape=1/sigma[k],scale=exp(mu[k]))
    p <- sapply(pleft,runif,n=1,max=1)
    los[pos] <- floor(qweibull(p,shape=1/sigma[k],scale=exp(mu[k])))
  }
  los
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
    ylb <- "Number of patients"
    y <- data$nhos
  }
  if(what=="ndead_daily"){
    tit <- "Number of daily deaths"
    ylb <- "Number of deaths"
    y <- if(!is.null(data$ndead)){data$ndead}else{NULL}
  }
  if(what=="ndead_cumul"){
    tit <- "Cumulative number of deaths"
    ylb <- "Number of deaths"
    y <- if(!is.null(data$ndead)){cumsum(data$ndead)}else{NULL}
  }

  par(mar=c(3,5,4,3),mgp=c(1.8,0.6,0))
  plot(range(days),range(c(y,Q)),type="n",xlab="",ylab="",las=1,xaxt="n")
  xdat <- conv(paste0("01.",unique(format(days,"%m.%Y"))),"%d.%m.%Y")
  axis.Date(side=1,at=xdat,format="%d.%m.%Y",hadj=0)
  ytck1 <- pretty(c(y,Q))
  ytck2 <- seq(min(ytck1),max(ytck1),by=diff(ytck1)[1]/5)
  abline(h=ytck1,col="grey",lwd=1)
  abline(h=ytck2,col="grey",lwd=0.5)
  abline(v=days,col="grey",lwd=0.5)
  abline(v=xdat,col="grey",lwd=1)
  mtext(ylb,side=2,line=2.8)
  title(tit)
  polygon(x=c(days[past],rev(days[past])),y=c(Q[past,1],rev(Q[past,3])),col="lightgrey",border=NA)
  lines(days[past],Q[past,2],lwd=2,col="darkgrey")
  polygon(x=c(days[futur],rev(days[futur])),y=c(Q[futur,1],rev(Q[futur,3])),col=rgb(rgb.blue,alpha=0.5),border=NA)
  lines(days[futur],Q[futur,2],lwd=2,col="red")
  points(data$date,y,pch=19,col="black")
  points(days[futur[-1]],Q[futur[-1],2],pch=19,col="red")
  abline(v=today,lty=2)
  mtext(format(today,"%d.%m.%Y"),side=3,at=today,line=0.2)
  abline(h=0)
  if(what%in%c("ndead_daily","ndead_cumul") & is.null(data$ndead)){
    legend("topleft",legend="Predicted counts",pch=19,col="red",bty="n",cex=1)
  } else {
    legend("topleft",legend=c("Observed counts","Predicted counts"),pch=c(19,19),col=c("black","red"),bty="n",cex=1)
  }
  mtext(paste0(100*prob,"%"),side=4,at=Q[nrow(Q),],cex=0.8,las=1,col=c(rgb(rgb.blue),"red",rgb(rgb.blue)),line=0.25)
}

# ------------------------------------------------------------------------------------------------------
# Import data. Input file can be either a file with date, nhos and nicu columns or a file with individual patient data
import.covid <- function(
  input.file="data.xlsx", # xlsx input data file
  start.date=NA,          # return counts only from this date onwards
  end.date=NA,            # return counts only up to this date
  date.format="%Y-%m-%d"  # date format in data file as well as in start.date and end.date
){

  # Detect file type (individual patient data or counts)
  sheets <- readxl::excel_sheets(input.file)
  nsheets <- length(sheets)
  for(k in 1:nsheets){
    raw <- as.data.frame(readxl::read_xlsx(input.file,sheet=k))
    if(colnames(raw)[1]=="no_unique"){type <- "ipd"; sheet <- k; break} # ipd=individual patient data file
    if(sum(colnames(raw)=="nhos")>0){type <- "agg"; sheet <- k; break}  # agg=aggregated data file
  }

  # Filter dates
  date <- if(type=="ipd"){conv(raw[,"arrivee_hopital"],date.format)}else{conv(raw$date,date.format)}
  start.date <- if(!is.na(start.date)){conv(start.date,format=date.format)}else{min(date,na.rm=T)}
  end.date <- if(!is.na(end.date)){conv(end.date,format=date.format)}else{max(date,na.rm=T)}
  sel <- which(date>=start.date & date<=end.date)
  raw <- raw[sel,]
  
  # Individual patient data
  if(type=="ipd"){
    # Define variables
    id <- as.character(raw[,"no_unique"])
    age <- as.numeric(raw[,"age"])
    sex <- factor(raw[,"sex"],levels=c("M","F"))
    hos_in <- conv(raw[,"arrivee_hopital"],date.format)
    icu_in <- conv(raw[,"debut_soins_intensifs"],date.format)
    icu_out <- conv(raw[,"fin_soins_intensifs"],date.format)
    hos_out <- conv(raw[,"sortie_hopital"],date.format)
    dead <- raw[,"deces"]
    dead[which(is.na(hos_out))] <- NA
    ipd <- data.frame(id=id,age=age,sex=sex,hos_in=hos_in,icu_in=icu_in,icu_out=icu_out,hos_out=hos_out,dead=dead)
    
    # Calculate daily cumulative count of hospitalized patients, daily nb of patients in ICU and daily nb of deaths
    days <- min(hos_in,na.rm=T)+c(0:diff(range(hos_in,na.rm=T)))
    ndays <- length(days)
    nhos <- nicu <- ndead <- numeric(ndays)
    for(j in 1:ndays){
      nhos[j] <- sum(hos_in<=days[j],na.rm=T)
      nicu[j] <- sum(icu_in<=days[j] & is.na(icu_out),na.rm=T) + sum(icu_in<=days[j] & icu_out>=days[j],na.rm=T)
      ndead[j] <- sum(dead==1 & hos_out==days[j],na.rm=T)
    }
    data <- data.frame(date=days,nhos=nhos,nicu=nicu,ndead=ndead)
  }
  
  # Aggregated data
  if(type=="agg"){
    # Filter dates
    ipd <- NULL
    data <- raw
    data$date <- conv(data$date, date.format)
  }
  data$nhos <- as.integer(data$nhos)
  data$nicu <- as.integer(data$nicu)
  if(!is.null(data$ndead)){data$ndead <- as.integer(data$ndead)}
  
  # Add attribute with individual patient data when available
  attr(data,"ipd") <- ipd
  data
}

# ------------------------------------------------------------------------------------------------------
# Forecast nb of ICU beds
pred.covid <- function(
  nday,      # nb of days to forecast
  nsim,      # nb of simulations
  pars,      # dataframe with parameters
  pars_surv, # dataframe with parameters for survival models (no user interaction!)
  data,      # dataframe with VD data
  type=NULL, # type of mortality predictions. NULL=automatic (based on data), 1=use IPD, 2=simulate from start but replace with IPD for past, 3=simulate from start with prediction interval on past
  ncpu,      # nb of parallel processes (use 1 for serial compiutations)
  seed=1234  # seed for reproducible computations
){
  t0 <- proc.time()
  set.seed(seed)
  
  # Check consistency of dates
  if(min(pars$date)>min(data$date)){stop("First date defining parameters must be anterior or equal to first date in the data!")}
  
  # Some useful things
  today <- data$date[nrow(data)]       # today i.e. last date entered in data
  days <- c(data$date,today+c(1:nday)) # vector of days for observed data and predictions
  j <- which(days==today)              # index of today
  ipd <- attr(data,"ipd")
  
  # Define prediction type (only applies to mortality!)
  if(is.null(type)){
    # Automatic detection based on data
    if(!is.null(ipd)){
      type <- 1 # use IPD
    } else {
      if(!is.null(data$ndead)){
        type <- 2 # simulate epidemic from start but replace simulated deaths with observed deaths in past
      } else {
        type <- 3 # simulate epidemic from start and provide prediction intervals for past
      }
    }
  } else {
    # Check type coherence
    if(type==1 & is.null(ipd)){stop("Predictions of type 1 cannot be used when 'data' do not contain individual patient data")}
    if(type==2 & is.null(data$ndead)){stop("Prediction of type 2 cannot be used in absence of mortality data")}
  }
  
  # Get parameters for each day in "days"
  ind <- sapply(days,function(d){max(which(pars$date<=d))})
  megp <- pars$megp[ind]
  vegp <- pars$vegp[ind]
  micp <- pars$micp[ind]
  vicp <- pars$vicp[ind]
  madp <- pars$madp[ind]
  vadp <- pars$vadp[ind]
  mlag <- pars$mlag[ind]
  vlag <- pars$vlag[ind]
  mlos <- pars$mlos[ind]
  vlos <- pars$vlos[ind]
  
  # Get parameters for survival models
  amin <- pars_surv$amin  
  page <- pars_surv$prob  # proportion of patients in each age category
  mu1 <- pars_surv$mu1
  sig1 <- pars_surv$sig1
  mu2 <- pars_surv$mu2
  sig2 <- pars_surv$sig2
  pfemale <- 0.4
  
  # Fill-in observed cumulative count of hospitalized patients
  nhos <- matrix(nrow=nsim,ncol=j+nday)
  nhos[,1:j] <- t(data$nhos)[rep(1,nsim),]
  
  # Predict future cumulative counts of hospitalized patients using exponential growth parameter
  for(k in (j+1):(j+nday)){nhos[,k] <- round(nhos[,k-1]*regp(nsim,megp[k],vegp[k]))}
  
  # Calculate daily new hospitalizations (i.e. incident counts)
  ninc <- matrix(nrow=nsim,ncol=j+nday)
  ninc[,1] <- nhos[,1]
  for(k in 1:(j+nday-1)){ninc[,k+1] <- nhos[,k+1]-nhos[,k]}
  if(sum(ninc<0)>0){stop("Some incident counts are negative!")}
  
  # Calculate incident cases that will require intensive cares at some point
  nicu <- matrix(nrow=nsim,ncol=j+nday)
  for(k in 1:(j+nday)){nicu[,k] <- round(ricp(nsim,micp[k],vicp[k])*ninc[,k])}
  
  # Calculate nb of ICU beds required (function to be parallelized)
  fun.nbed <- function(s){
    set.seed(seed+s*10)
    npat <- nicu[s,] # nb of patients that will require ICU at some point for each hospitalization day
    
    hos.in <- unlist(mapply(rep,x=1:(j+nday),times=npat,SIMPLIFY=FALSE))     # define hospitalization day (before ICU) for these patients
    lag <- unlist(mapply(rlag,n=npat,mlag=mlag,vlag=vlag,SIMPLIFY=FALSE))    # ICU lag for these patients
    los <- unlist(mapply(rlos,n=npat,mlos=mlos,vlos=vlos,SIMPLIFY=FALSE))    # length of stay in ICU for these patients
    
    # Define theoretical ICU day-in and day-out for these patients
    icu.in <- hos.in+lag
    icu.out <- icu.in+los
    
    # Restrict ICU admission to a subset of patients that should ideally enter ICU on day k
    adp <- unlist(mapply(radp,n=1,madp=madp,vadp=vadp,SIMPLIFY=FALSE)) # simulated admission probability on each day
    restrict <- which(adp<1) # index of days for which a restriction on ICU admission should be applied
    if(length(restrict)>0){
      for(k in restrict){
        sel <- which(icu.in==k); nsel <- length(sel) # select patients that are supposed to enter ICU on day k
        if(nsel>0){
          admit <- rbinom(nsel,size=1,prob=adp[k])   # patient-specific binary indicator for effective ICU admission on day k
          icu.in[sel] <- icu.in[sel]*admit           # all patients with icu.in=0 are NOT admitted
          icu.out[sel] <- icu.out[sel]*admit         # all patients with icu.out=0 are NOT admitted
        }
      }
    }
    
    # Restrict attention to patients that will be effectively admitted in ICU
    icu.in <- icu.in[icu.in>0]
    icu.out <- icu.out[icu.out>0]
    
    # Fill-in bed occupancy matrix in ICU
    occ <- matrix(FALSE,nrow=length(icu.in),ncol=j+nday)
    for(i in 1:nrow(occ)){occ[i,which(c(1:(j+nday))%in%c(icu.in[i]:icu.out[i]))] <- TRUE}
    
    # Return daily nb of occupied ICU beds
    apply(occ,2,sum)
  }
  
  # Calculate nb of daily deaths (function to be parallelized)
  fun.ndead <- function(s){
    set.seed(seed+s*10)
    npat <- ninc[s,] # nb of new patients hospitalized each day
    
    if(type==1){
      # Use individual patient data to reconstruct history
      future <- j+c(1:nday)
      Nfuture <- sum(npat[future])

      agecat <- c(as.numeric(cut(ipd$age,breaks=c(amin,Inf),include.lowest=TRUE)),ragecat(Nfuture,page))
      sex <- c(ipd$sex,rsex(Nfuture,pfemale))
      hos.in <- c(ipd$hos_in,do.call("c",mapply(rep,x=days[future],times=npat[future],SIMPLIFY=FALSE)))
      hos.out <- c(ipd$hos_out,rep(NA,Nfuture))
      los <- as.numeric(hos.out-hos.in)
      dead <- c(ipd$dead,rep(NA,Nfuture))

      # Simulate LOS for existing patients that are still in hospital: LOS is censored tomorrow
      sel <- which(hos.in<=today & is.na(hos.out))
      if(length(sel)>0){
        los[sel] <- sim.los(agecat[sel],mu2,sig2,left=as.numeric(today-hos.in[sel])+1)
        hos.out[sel] <- hos.in[sel]+los[sel]
      }

      # Simulate LOS for new patients
      sel <- which(hos.in>today & is.na(hos.out))
      if(length(sel)>0){
        los[sel] <- sim.los(agecat[sel],mu2,sig2,left=NULL)
        hos.out[sel] <- hos.in[sel]+los[sel]
      }

      # Calculate probability of death for patients without death status
      sel <- which(is.na(dead))
      if(length(sel)>0){
        h1 <- pred.h(los[sel],agecat[sel],mu=mu1,sigma=sig1) # hazard of dying
        h2 <- pred.h(los[sel],agecat[sel],mu=mu2,sigma=sig2) # hazard of exiting (dead or alive)
        pdead <- h1/h2                                       # probability of dying at the end of LOS
        dead[sel] <- sapply(pdead,rbinom,n=1,size=1)         # death indicator
      }
    } else {
      # Simulate fictive deaths from start of epidemy
      N <- sum(npat)
      agecat <- ragecat(N,page)
      sex <- rsex(N,pfemale)
      hos.in <- do.call("c",mapply(rep,x=days,times=npat,SIMPLIFY=FALSE))
      los <- sim.los(agecat,mu2,sig2)
      hos.out <- hos.in+los
      h1 <- pred.h(los,agecat,mu1,sig1)       # hazard of dying
      h2 <- pred.h(los,agecat,mu2,sig2)       # hazard of exiting (dead or alive)
      pdead <- h1/h2                          # probability of dying at the end of LOS
      dead <- sapply(pdead,rbinom,n=1,size=1) # death indicator
    }
    
    # Calculate daily nb of deaths
    ndead <- integer(j+nday)
    for(k in 1:(j+nday)){ndead[k] <- sum(dead[which(hos.out==days[k])])}
    
    # In absence of IPD, replace historic simulated death counts with observed death counts when available (crude fix)
    if(type==2){ndead[1:j] <- data$ndead}
    ndead
  }
  
  # Run serial/parallel calculations
  if(ncpu>1){
    require(snowfall)
    sfInit(parallel=TRUE, cpus=ncpu)
    sfExportAll() 
    nbed <- t(sfSapply(1:nsim, fun.nbed))
    ndead <- t(sfSapply(1:nsim, fun.ndead))
    sfStop()
  } else {
    nbed <- ndead <- matrix(nrow=nsim,ncol=j+nday)
    for(s in 1:nsim){
      cat("Progress: ",round(100*s/nsim),"%\r",sep=""); flush.console()
      nbed[s,] <- fun.nbed(s)
      ndead[s,] <- fun.ndead(s)
    }
  }
  colnames(nhos) <- colnames(ninc) <- colnames(nbed) <- colnames(ndead) <- format(days,format="%d.%m.%Y")
  ndead_cumul <- t(apply(ndead,1,cumsum))
  
  t1 <- proc.time()
  cat("Calculations completed in",ceiling((t1-t0)[3]),"seconds","\n"); flush.console()
  list(
    nhos=nhos,   # cumulative counts of hospitalized patients
    ninc=ninc,   # daily new hopitalized patients
    nbed=nbed,   # predicted nb of occupied ICU beds
    ndead_daily=ndead,       # predicted nb of daily deaths
    ndead_cumul=ndead_cumul, # predicted cumulative number of deaths
    data=data    # return data (for plotting)
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

fit.wb <- function(
  x,          # count vector (e.g. nb of days)
  cens=NULL   # binary indicator for right-censored (1) or observed (0) data. If NULL, x is fully observed
){
  x <- as.numeric(x)
  if(is.null(cens)){cens <- 0*x}
  
  # Remove missing values
  keep <- which(!is.na(x))
  x <- x[keep]
  cens <- cens[keep]
  
  # Create bounds for interval censoring
  left <- x
  right <- x+1
  right[cens==1] <- Inf
  
  # Define interval-censoring log-likelihood for weibull distribution
  # This may still be subject to numerical issue with ll=-Inf when P(right)=P(left) although right>left
  wb.llik <- function(pars,left,right){
    a <- exp(pars[1])
    b <- exp(pars[2])
    ll <- log(pweibull(right,shape=a,scale=b)-pweibull(left,shape=a,scale=b))
    sum(ll)
  }
  
  # Initial values
  # see https://stats.stackexchange.com/questions/230937/how-to-find-initial-values-for-weibull-mle-in-r
  xs <- sort(x+1e-06)
  Fh <- ppoints(xs)
  a0 <- lm(log(-log(1-Fh))~log(xs))$coefficients[2]
  b0 <- quantile(x,prob=0.632)
  
  # Optimisation
  opt <- optim(par=c(log(a0),log(b0)),fn=wb.llik,left=left,right=right,method="BFGS",control=list(fnscale=-1,maxit=1000))

  list(shape=exp(opt$par[1]),scale=exp(opt$par[2]),loglik=opt$value)
}

pbccg <- function(q,mu,sigma,lambda){
  if(lambda==0){
    zt <- log(q/mu)/sigma
  } else {
    zt <- ((q/mu)^lambda-1)/(lambda*sigma)
  }
  ptr <- pnorm(-1/(sigma*abs(lambda))) # truncation probability
  p <- if(lambda<=0){pnorm(z)/(1-ptr)}else{(pnorm(z)-ptr)/(1-ptr)}
  p
}

qbccg <- function(p,mu,sigma,lambda){
  zp <- qnorm(p)
  ptr <- pnorm(-1/(sigma*abs(lambda))) # truncation probability
  zt <- if(lambda<=0){qnorm(p*(1-ptr))}else{qnorm(p*(1-ptr)+ptr)}
  if(lambda==0){
    q <- mu*exp(zt*sigma)
  } else {
    q <- mu*abs(1+zt*lambda*sigma)^(1/lambda)
  }
  q
}

fit.bccg <- function(
  x,
  cens=NULL
){
  if(is.null(cens)){cens <- rep(0,length(x))}
  bccg.llik <- function(pars,x,cens){
    mu <- exp(pars[1])
    sig <- exp(pars[2])
    lam <- pars[3]
    z <- x2z(x,mu,sig,lam)
    ljac <- -log(mu)-log(sig)+(lam-1)*(log(x)-log(mu))
    ll <- x*0
    obs <- which(cens==0)
    cns <- which(cens==1)
    if(length(obs)>0){ll[obs] <- dnorm(z[obs],log=TRUE)+ljac[obs]}
    if(length(cns)>0){ll[cns] <- pnorm(z[cns],lower.tail=FALSE,log.p=TRUE)}
    sum(ll)
  }
  ini <- c(log(median(x)),log(sd(log(x))),0)
  opt <- optim(par=ini,fn=bccg.llik,x=x,cens=cens,method="BFGS",control=list(fnscale=-1,maxit=1000))
  mu <- exp(opt$par[1])
  sigma <- exp(opt$par[2])
  lambda <- opt$par[3]
  list(mu=mu,sigma=sigma,lambda=lambda,loglik=opt$value)
}


# -----------------------------------------------------------------------------
# Design matrix for fractional polynomials
# see  https://www.jstor.org/stable/2986270
# scaling defined as in https://rdrr.io/cran/mfp/src/R/fp.scale.R
FP <- function(x,p,shift=NULL,scale=NULL){
  if(is.null(shift)){
    xmin <- min(x,na.rm=TRUE)
    if(xmin<=0){
      z <- diff(sort(x))
      shift <- min(z[z > 0]) - xmin
      shift <- ceiling(shift*10)/10
    } else {
      shift <- 0
    }
  }
  x <- x+shift
  if(is.null(scale)){
    range <- max(x,na.rm=T) - min(x,na.rm=T)
    scale <- 10^(sign(log10(range)) * round(abs(log10(range))))
  }
  x <- x/scale
  p <- as.numeric(p)
  m <- length(p)
  pp <- c(0,p)
  X <- matrix(nrow=length(x), ncol=m)
  for(i in 1:m){X[,i] <- if(p[i]==0){log(x)}else{x^p[i]}}
  H <- matrix(nrow=length(x), ncol=m+1)
  H[,1] <- 1
  for(i in 2:(m+1)){H[,i] <- if(pp[i]==pp[i-1]){H[,i-1]*log(x)}else{X[,i-1]}}
  H <- H[,-1,drop=FALSE] # remove intercept
  attr(H,"shift") <- shift
  attr(H,"scale") <- scale
  attr(H,"p") <- p
  H
}

best.FP <- function(model,xform,degree=2){
  form <- as.character(formula(model))
  xvar <- as.character(xform)[2]
  
  pgrid <- as.matrix(c(-2,-1,-0.5,0,0.5,1,2,3))
  if(degree==2){
    pgrid <- expand.grid(pgrid,pgrid)
    pgrid <- pgrid[which(pgrid[,2]>=pgrid[,1]),]
  }

  fm.grid <- rep(list(NULL),nrow(pgrid))
  for(k in 1:nrow(pgrid)){
    formk <- paste0(form[2],"~",gsub(xvar,paste0("FP(",xvar,",c(",paste0(pgrid[k,],collapse=","),"))"),form[3],fixed=TRUE))
    fm.grid[[k]] <- update(model,formula=as.formula(formk))
  }
  LL.grid <- sapply(fm.grid,logLik)
  best <- rev(order(LL.grid))[1]
  model <- fm.grid[[best]]
  power <- as.numeric(pgrid[best,])
  LL <- LL.grid[best]
  npar <- length(coef(model))+degree
  aic <- -2*LL+2*npar
  list(model=model,power=power,loglik=LL,aic=aic)
}
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
  n,         # nb draws
  mlag,      # expected value for lag (in days)
  vlag       # variability
){rnbinom(n,mu=mlag,size=mlag^2/(vlag-mlag))}

# Using negative binomial distribution
rlag2 <- function(
  mlag,
  vlag,
  lower=NULL,
  upper=NULL
){
  n <- length(mlag)
  if(is.null(lower)){lower <- rep(0,n)}
  if(is.null(upper)){upper <- rep(Inf,n)}
  plower <- unlist(mapply(pnbinom,q=lower,mu=mlag,size=mlag^2/(vlag-mlag),SIMPLIFY=FALSE))
  pupper <- unlist(mapply(pnbinom,q=upper,mu=mlag,size=mlag^2/(vlag-mlag),SIMPLIFY=FALSE))
  p <- unlist(mapply(runif,n=1,min=plower,max=pupper,SIMPLIFY=FALSE))
  unlist(mapply(qnbinom,p=p,mu=mlag,size=mlag^2/(vlag-mlag),SIMPLIFY=FALSE))
}

# ------------------------------------------------------------------------------------------------------
# Random sample for ICU length of stay (LOS)
rlos <- function(
  n,    # nb draws
  mlos, # expected value for length of stay (in days)
  vlos  # variability
){rnbinom(n,mu=mlos,size=mlos^2/(vlos-mlos))}

# Using weibull distribution
rlos2 <- function(
  mu,
  sigma,
  lower=NULL,
  upper=NULL
){
  n <- length(mu)
  if(is.null(lower)){lower <- rep(0,n)}
  if(is.null(upper)){upper <- rep(Inf,n)}
  plower <- unlist(mapply(pweibull,q=lower,shape=1/sigma,scale=exp(mu),SIMPLIFY=FALSE))
  pupper <- unlist(mapply(pweibull,q=upper,shape=1/sigma,scale=exp(mu),SIMPLIFY=FALSE))
  p <- unlist(mapply(runif,n=1,min=plower,max=pupper,SIMPLIFY=FALSE))
  floor(unlist(mapply(qweibull,p=p,shape=1/sigma,scale=exp(mu),SIMPLIFY=FALSE)))
}

# ------------------------------------------------------------------------------------------------------
# Generate dataframe with age and sex for fictive patients
rpop <- function(
  n,       # total nb of patients                     
  breaks,  # breaks for age categories
  page,    # proportion of patients in each age category (should sum-up to 1)
  pfem     # proportion of females in each age category
){
  stopifnot(length(page)==length(pfem))
  stopifnot(length(breaks)==(length(page)+1))
  stopifnot(sum(page)==1)
  ncat <- length(page)
  nT <- round(n*page); nT[ncat] <- n-sum(nT[1:(ncat-1)]) # nb patients in each age category
  nF <- round(nT*pfem)                                   # nb females in each age category
  nM <- nT-nF                                            # nb males in each age category
  
  # Uniform age within categories
  ageF <- ceiling(as.numeric(unlist(mapply(runif,n=nF,min=breaks[1:ncat],max=breaks[2:(ncat+1)],SIMPLIFY=FALSE))))
  ageM <- ceiling(as.numeric(unlist(mapply(runif,n=nM,min=breaks[1:ncat],max=breaks[2:(ncat+1)],SIMPLIFY=FALSE))))
  age <- c(ageM,ageF)
  sex <- factor(c(rep("M",sum(nM)),rep("F",sum(nF))),levels=c("M","F"))
  
  # # Resample age in data
  # ipd <- attr(data,"ipd")
  # ageM <- sample(ipd$age[ipd$sex=="M"],size=sum(nM),replace=TRUE)
  # ageF <- sample(ipd$age[ipd$sex=="F"],size=sum(nF),replace=TRUE)
  # age <- c(ageM,ageF)
  # sex <- factor(c(rep("M",sum(nM)),rep("F",sum(nF))),levels=c("M","F"))
  
  # # Categorical age
  # ageM <- as.character(unlist(mapply(rep,x=names(nM),times=nM,SIMPLIFY=FALSE)))
  # ageF <- as.character(unlist(mapply(rep,x=names(nF),times=nF,SIMPLIFY=FALSE)))
  # age <- factor(c(ageM,ageF),levels=names(page))
  # sex <- factor(c(rep("M",sum(nM)),rep("F",sum(nF))),levels=c("M","F"))

  data.frame(age=age,sex=sex)
}

# ------------------------------------------------------------------------------------------------------
# Define RGB code for shiny blue (to be used in rgb() function possibly with alpha transparency)
rgb.blue <- t(col2rgb("#428bca"))/255

# ------------------------------------------------------------------------------------------------------
# Histogram
histo <- function(x,prob=NULL,ylim=NULL){
  if(var(x)>0){
    brks <- attr(x,"breaks")
    if(is.null(brks)){
      hist(x,breaks=30,xlab="",ylab="",main="",col=rgb(rgb.blue,alpha=0.5),yaxt="n",yaxs="i",freq=FALSE,ylim=ylim)
    } else {
      # hist(x,breaks=brks,xlab="",ylab="",main="",col=rgb(rgb.blue,alpha=0.5),xaxt="n",yaxt="n",yaxs="i",freq=FALSE)
      # axis(side=1,at=brks)
      xcat <- cut(x,breaks=brks,include.lowest=T)
      barplot(table(xcat)/length(x),ylab="",col=rgb(rgb.blue,alpha=0.5),yaxt="n",ylim=ylim)
    }
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

# ------------------------------------------------------------------------------------------------------
# Plot function for object returned by pred.covid()
plot.covid <- function(
  object.covid,            # object returned by pred.covid()
  what="nbed",             # name of element to plot in object.covid
  prob=c(0.025,0.5,0.975), # quantiles to plot (length=3!)
  from=NULL,               # date from which plot is displayed (as string)
  to=NULL,                 # date up to which plot is displayed (as string)
  date.format="%m/%d/%Y"   # date format for 'from' and 'to'
){
  stopifnot(length(prob)==3)
  data <- object.covid$data
  X <- object.covid[[what]]
  days <- as.Date(strptime(colnames(X),format="%d.%m.%Y"))
  today <- data$date[nrow(data)]
  
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
  
  # Apply date filtering
  if(!is.null(from)){
    from <- conv(from,date.format)
    pos1 <- which(data$date>=from)
    pos2 <- which(days>=from)
    data <- data[pos1,,drop=F]
    y <- y[pos1]
    X <- X[,pos2,drop=F]
    days <- days[pos2]
  }
  if(!is.null(to)){
    to <- conv(to,date.format)
    pos1 <- which(data$date<=to)
    pos2 <- which(days<=to)
    data <- data[pos1,,drop=F]
    y <- y[pos1]
    X <- X[,pos2,drop=F]
    days <- days[pos2]
  }
  
  Q <- t(apply(X,2,quantile,probs=prob))
  past <- which(days<=today)
  futur <- which(days>=today)
  
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
  polygon(x=c(days[past],rev(days[past])),y=c(Q[past,1],rev(Q[past,3])),col=rgb(t(col2rgb("lightgrey"))/255,alpha=0.5),border=NA)
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
  date.format="%m/%d/%Y"  # date format in data file as well as in start.date and end.date
){

  # Detect file type (individual patient data or counts)
  sheets <- readxl::excel_sheets(input.file)
  nsheets <- length(sheets)
  for(k in 1:nsheets){
    raw <- as.data.frame(readxl::read_xlsx(input.file,sheet=k))
    if(colnames(raw)[1]=="no_unique"){dat.type <- "ipd"; sheet <- k; break} # ipd=individual patient data file
    if(sum(colnames(raw)=="nhos")>0){dat.type <- "agg"; sheet <- k; break}  # agg=aggregated data file
  }

  # Filter dates
  date <- if(dat.type=="ipd"){conv(raw[,"arrivee_hopital"],date.format)}else{conv(raw$date,date.format)}
  if (any(is.na(date))) {
    warning("Missing dates or wrong date format")
    return(data.frame(date=numeric(0),nhos=numeric(0),nicu=numeric(0)))
  }
  start.date <- if(!is.na(start.date)){conv(start.date,format=date.format)}else{min(date,na.rm=T)}
  end.date <- if(!is.na(end.date)){conv(end.date,format=date.format)}else{max(date,na.rm=T)}
  sel <- which(date>=start.date & date<=end.date)
  ndel <- if(dat.type=="agg" & sel[1]>1){raw[sel[1]-1,"nhos"]}else{0} # cumulative nb of hospitalizations just before start.date (these will be removed)
  raw <- raw[sel,]
  
  # Individual patient data
  if(dat.type=="ipd"){
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
    days <- start.date+c(0:(end.date-start.date))
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
  if(dat.type=="agg"){
    # Filter dates
    ipd <- NULL
    data <- raw
    data$nhos <- data$nhos-ndel # remove hospitalizations that occurred before start.date
    data$date <- conv(data$date, date.format)
  }
  data$nhos <- as.integer(data$nhos)
  data$nicu <- as.integer(data$nicu)
  if(!is.null(data$ndead)){data$ndead <- as.integer(data$ndead)}
  
  # Defined allowed prediction types for mortality
  if(dat.type=="ipd"){type <- c(1,2,3)}
  if(dat.type=="agg" & !is.null(data$ndead)){type <- c(2,3)}
  if(dat.type=="agg" & is.null(data$ndead)){type <- 3}

  # Add attribute with individual patient data (when available) and allowed prediction type
  attr(data,"ipd") <- ipd
  attr(data,"type") <- type
  data
}

# ------------------------------------------------------------------------------------------------------
# Load parameters and population characteristics
pars.covid <- function(
  input.file="params.xlsx",  # file with parameters and age/sex distributions
  date.format="%d.%m.%Y"     # date format
){
  require(readxl)
  
  # Load parameters
  params <- as.data.frame(read_xlsx(input.file,sheet="params"))
  params$date <- conv(params$date,date.format)
  
  # Load age distribution
  age_dist <- as.data.frame(read_xlsx(input.file,sheet="age_distrib"))
  age_dist$date <- conv(age_dist$date,date.format)
  
  # Check that proportions of patients in each age category sum up to one
  chk <- which(!apply(age_dist[,-1],1,sum)==1)
  if(length(chk)>0){stop("Some dates include proportions of patients in each age category that do not sum-up to one!")}
  
  # Load sex distribution
  sex_dist <- as.data.frame(read_xlsx(input.file,sheet="sex_distrib"))
  sex_dist$date <- conv(sex_dist$date,date.format)
  
  # Define breaks for age categories
  agecat <- colnames(age_dist)[-1]
  lower <- sapply(strsplit(agecat,split="-"),function(x){min(as.numeric(x))})
  upper <- sapply(strsplit(agecat,split="-"),function(x){max(as.numeric(x))})
  breaks <- c(lower[1],upper)
  
  list(params=params,age_dist=age_dist,sex_dist=sex_dist,age.breaks=breaks)
}

# ------------------------------------------------------------------------------------------------------
# Forecast nb of ICU beds
pred.covid <- function(
  nday,       # nb of days to forecast
  nsim,       # nb of simulations
  pars,       # list with parameters and age/sex distributions (as returned by pars.covid)
  data,       # dataframe with VD data (as returned by import.covid)
  mort,       # list with formulas and coefficients for mortality models (usually retrived from load("mort.Rdata"))
  type=NULL,  # type of mortality predictions. NULL=automatic (based on data), 1=use IPD, 2=simulate from start but replace with IPD for past, 3=simulate from start with prediction interval on past
  ncpu=4,     # nb of parallel processes (use 1 for serial computations)
  seed=1234,  # seed for reproducible computations
  vcov=FALSE  # draw coefficients of mortality models in their sampling distribution (i.e. using the estimated variance-covariance matrix)
){
  t0 <- proc.time()
  set.seed(seed)
  
  # Check consistency of dates
  if(min(pars$params$date)>min(data$date)){stop("First date defining parameters must be anterior or equal to first date in the data!")}
  if(min(pars$age_dist$date)>min(data$date)){stop("First date defining proportion of patients in each age category must be anterior or equal to first date in the data!")}
  if(min(pars$sex_dist$date)>min(data$date)){stop("First date defining proportion of females in each age category must be anterior or equal to first date in the data!")}
  
  # Some useful things
  today <- data$date[nrow(data)]       # today i.e. last date entered in data
  days <- c(data$date,today+c(1:nday)) # vector of days for observed data and predictions
  j <- which(days==today)              # index of today
  ipd <- attr(data,"ipd")              # extract individual patient data when available
  age.breaks <- pars$age.breaks        # breaks for age categories

  # Define prediction type (only applies to mortality!)
  allowed <- attr(data,"type")
  if(is.null(type)){type <- min(allowed)}
  if(type<min(allowed)){stop("Chosen mortality prediction type is incompatible with data")}
  
  # Get parameters for each day in "days"
  ind <- sapply(days,function(d){max(which(pars$params$date<=d))})
  megp <- pars$params$megp[ind]
  vegp <- pars$params$vegp[ind]
  micp <- pars$params$micp[ind]
  vicp <- pars$params$vicp[ind]
  madp <- pars$params$madp[ind]
  vadp <- pars$params$vadp[ind]
  mlag <- pars$params$mlag[ind]
  vlag <- pars$params$vlag[ind]
  mlos <- pars$params$mlos[ind]
  vlos <- pars$params$vlos[ind]
  
  # Get proportion of patients in each age category for each day in "days"
  ind <- sapply(days,function(d){max(which(pars$age_dist$date<=d))})
  page <- as.matrix(pars$age_dist[ind,-1])
  
  # Get proportion of females in each age category for each day in "days"
  ind <- sapply(days,function(d){max(which(pars$sex_dist$date<=d))})
  pfem <- as.matrix(pars$sex_dist[ind,-1])
  
  # Coefficients of mortality models
  if(vcov){
    # Draw coefficients from their sampling distributions
    cf.icp <- MASS::mvrnorm(nsim,mort$icp$coef,mort$icp$vcov)
    cf.dead <- MASS::mvrnorm(nsim,mort$dead$coef,mort$dead$vcov)
    cf.exit <- MASS::mvrnorm(nsim,mort$exit$coef,mort$exit$vcov)
  } else {
    # Use point estimated in all simulations
    cf.icp <- t(mort$icp$coef)[rep(1,nsim),]
    cf.dead <- t(mort$dead$coef)[rep(1,nsim),]
    cf.exit <- t(mort$exit$coef)[rep(1,nsim),]
  }
    
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
  
  # Calculate nb of ICU beds required (function to be parallelized)
  fun.nbed <- function(s){
    set.seed(seed+s*10)
    icp <- unlist(mapply(ricp,n=1,micp=micp,vicp=vicp,SIMPLIFY=FALSE))
    npat <- round(icp*ninc[s,]) # nb of patients that will require ICU at some point for each hospitalization day

    pos <- which(npat>0) # only consider days with new ICU patients (speeds-up calculations)
    hos.in <- unlist(mapply(rep,x=c(1:(j+nday))[pos],times=npat[pos],SIMPLIFY=FALSE))       # define hospitalization day (before ICU) for these patients
    lag <- unlist(mapply(rlag,n=npat[pos],mlag=mlag[pos],vlag=vlag[pos],SIMPLIFY=FALSE))    # ICU lag for these patients
    los <- unlist(mapply(rlos,n=npat[pos],mlos=mlos[pos],vlos=vlos[pos],SIMPLIFY=FALSE))    # length of stay in ICU for these patients
    
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
      # Restrict attention to patients that will be effectively admitted in ICU
      icu.in <- icu.in[icu.in>0]
      icu.out <- icu.out[icu.out>0]
    }
    
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
      
      # Define known IC requirement status
      pos0 <- which(is.na(ipd$icu_in) & !is.na(ipd$hos_out)) # existing patients who left hospital (alive or dead) without going through ICU => ic=0
      pos1 <- which(!is.na(ipd$icu_in)) # existing patients who were admitted in ICU => ic=1
      ipd$ic <- rep(NA,nrow(ipd))
      ipd$ic[pos0] <- 0
      ipd$ic[pos1] <- 1
      
      # Draw age and sex for future patients
      new <- do.call("rbind",lapply(future,function(k){rpop(npat[k],breaks=age.breaks,page=page[k,],pfem=pfem[k,])}))
      
      # Rebuild complete data (with some missing values)
      all <- data.frame(
        age=c(ipd$age,new$age),
        sex=unlist(list(ipd$sex,new$sex)),
        ic=c(ipd$ic,rep(NA,Nfuture)),
        hos.in=c(ipd$hos_in,do.call("c",mapply(rep,x=days[future],times=npat[future],SIMPLIFY=FALSE))),
        icu.in=c(ipd$icu_in,rep(NA,Nfuture)),
        lag=c(as.numeric(ipd$icu_in-ipd$hos_in),rep(NA,Nfuture)),
        hos.out=c(ipd$hos_out,rep(NA,Nfuture)),
        los=c(as.numeric(ipd$hos_out-ipd$hos_in),rep(NA,Nfuture)),
        pdead=c(ipd$dead,rep(NA,Nfuture)),
        dead=c(ipd$dead,rep(NA,Nfuture))
      )
      
      # Impute missing IC status based on age/sex using ICP model
      sel <- which(is.na(all$ic))
      if(length(sel)>0){
        X <- model.matrix(xform(mort$icp$formula),all[sel,])
        pic <- expit(as.numeric(X%*%cf.icp[s,]))
        all$ic[sel] <- sapply(pic,rbinom,n=1,size=1)
      }
      
      # Simulate hospital LOS for existing patients that are still in hospital: LOS is censored tomorrow
      sel <- which(all$hos.in<=today & is.na(all$hos.out))
      if(length(sel)>0){
        X <- model.matrix(xform(mort$exit$formula),all[sel,])
        mu <- as.numeric(X%*%cf.exit[s,-ncol(cf.exit)])
        sigma <- exp(cf.exit[s,ncol(cf.exit)])
        all$los[sel] <- rlos2(mu,sigma,lower=as.numeric(today-all$hos.in[sel])+1)
        all$hos.out[sel] <- all$hos.in[sel]+all$los[sel]
      }

      # Simulate LOS for new patients
      sel <- which(all$hos.in>today & is.na(all$hos.out))
      if(length(sel)>0){
        X <- model.matrix(xform(mort$exit$formula),all[sel,])
        mu <- as.numeric(X%*%cf.exit[s,-ncol(cf.exit)])
        sigma <- exp(cf.exit[s,ncol(cf.exit)])
        all$los[sel] <- rlos2(mu,sigma)
        all$hos.out[sel] <- all$hos.in[sel]+all$los[sel]
      }
      
      # Simulate lag for patients requiring IC (with condition lag<=LOS)
      sel <- which(all$ic==1 & is.na(all$icu.in)); nsel <- length(sel)
      if(nsel>0){
        all$lag[sel] <- rlag2(mlag=rep(mort$lag$mlag,nsel),vlag=rep(mort$lag$vlag,nsel),upper=all$los[sel])
        all$icu.in[sel] <- all$hos.in[sel]+all$lag[sel]
      }

      # Calculate probability of death at the end of hospital stay for patients without death status
      sel <- which(is.na(all$dead))
      if(length(sel)>0){

        # Predict hazard of death
        X1 <- model.matrix(xform(mort$dead$formula),all[sel,])
        mu.dead <- as.numeric(X1%*%cf.dead[s,-ncol(cf.dead)])
        sigma.dead <- exp(cf.dead[s,ncol(cf.dead)])
        h.dead <- pred.weibull(all$los[sel],mu.dead,sigma.dead,what="hazard")
        
        # Predict hazard of exit
        X2 <- model.matrix(xform(mort$exit$formula),all[sel,])
        mu.exit <- as.numeric(X2%*%cf.exit[s,-ncol(cf.exit)])
        sigma.exit <- exp(cf.exit[s,ncol(cf.exit)])
        h.exit <- pred.weibull(all$los[sel],mu.exit,sigma.exit,what="hazard")
        
        # Calculate probability of dying at the end of hospital stay (forced<=1)
        all$pdead[sel] <- pmin(h.dead/h.exit,1)
      }
      
      # Restrict ICU admission
      adp <- unlist(mapply(radp,n=1,madp=madp,vadp=vadp,SIMPLIFY=FALSE)) # simulated admission probability on each day
      restrict <- which(adp<1) # index of days for which a restriction on ICU admission should be applied
      if(length(restrict)>0){
        for(k in restrict){
          sel <- which(all$icu.in==days[k]); nsel <- length(sel) # select patients that are supposed to enter ICU on day k
          if(nsel>0){
            refused <- rbinom(nsel,size=1,prob=1-adp[k])            # patient-specific binary indicator for effective ICU refusal on day k

            # nref <- round(nsel*(1-adp[k]))
            # refused <- rep(0,nsel)
            # refused[sample(c(1:nsel),size=nref)] <- 1
  
            
            # # Predict hazard of death on day k
            # X1 <- model.matrix(xform(mort$dead$formula),all[sel,,drop=FALSE])
            # mu.dead <- as.numeric(X1%*%cf.dead[s,-ncol(cf.dead)])
            # sigma.dead <- exp(cf.dead[s,ncol(cf.dead)])
            # h.dead <- pred.weibull(all$lag[sel],mu.dead,sigma.dead,what="hazard")
            # 
            # # Predict hazard of exit on day k
            # X2 <- model.matrix(xform(mort$exit$formula),all[sel,,drop=FALSE])
            # mu.exit <- as.numeric(X2%*%cf.exit[s,-ncol(cf.exit)])
            # sigma.exit <- exp(cf.exit[s,ncol(cf.exit)])
            # h.exit <- pred.weibull(all$lag[sel],mu.exit,sigma.exit,what="hazard")
            # 
            # # Calculate probability of dying on day k
            # pdead <- pmin(h.dead/h.exit,1)

            sel.refused <- sel[refused==1]                        # among patients requiring IC on day k, select those that will be refused
            all$hos.out[sel.refused] <- all$icu.in[sel.refused]   # set date of hospital exit on the theoretical date of ICU admission
            all$los[sel.refused] <- all$lag[sel.refused] #as.numeric(all$hos.out[sel.refused]-all$hos.in[sel.refused]) # correct LOS
            all$pdead[sel.refused] <- 1                           # set probability of dying to 1 for patients refused in ICU
          }
        }
      }
      
      # Generate death indicator
      sel <- which(is.na(all$dead))
      all$dead[sel] <- sapply(all$pdead[sel],rbinom,n=1,size=1)
    } else {
      # Simulate fictive deaths from start of epidemy
      N <- sum(npat)
      pos <- which(npat>0) # only consider days with new hospitalized patients
      
      # Generate age and sex
      all <- do.call("rbind",lapply(pos,function(k){rpop(npat[k],breaks=age.breaks,page=page[k,],pfem=pfem[k,])}))

      # Draw IC status using ICP model
      X <- model.matrix(xform(mort$icp$formula),all)
      pic <- expit(as.numeric(X%*%cf.icp[s,]))
      all$ic <- sapply(pic,rbinom,n=1,size=1)
      
      # Define hospital entrance
      all$hos.in <- do.call("c",mapply(rep,x=days[pos],times=npat[pos],SIMPLIFY=FALSE))
      
      # Draw total LOS
      X <- model.matrix(xform(mort$exit$formula),all)
      mu <- as.numeric(X%*%cf.exit[s,-ncol(cf.exit)])
      sigma <- exp(cf.exit[s,ncol(cf.exit)])
      all$los <- rlos2(mu,sigma)
      all$hos.out <- all$hos.in+all$los
      
      # Simulate lag for patients requiring IC (with condition lag<=LOS)
      all$icu.in <- as.Date(rep(NA,nrow(all)))
      all$lag <- rep(NA,nrow(all))
      sel <- which(all$ic==1); nsel <- length(sel)
      if(nsel>0){
        all$lag[sel] <- rlag2(mlag=rep(mort$lag$mlag,nsel),vlag=rep(mort$lag$vlag,nsel),upper=all$los[sel])
        all$icu.in[sel] <- all$hos.in[sel]+all$lag[sel]
      }

      # Predict hazard of death
      X1 <- model.matrix(xform(mort$dead$formula),all)
      mu.dead <- as.numeric(X1%*%cf.dead[s,-ncol(cf.dead)])
      sigma.dead <- exp(cf.dead[s,ncol(cf.dead)])
      h.dead <- pred.weibull(all$los,mu.dead,sigma.dead,what="hazard")
      
      # Predict hazard of exit
      X2 <- model.matrix(xform(mort$exit$formula),all)
      mu.exit <- as.numeric(X2%*%cf.exit[s,-ncol(cf.exit)])
      sigma.exit <- exp(cf.exit[s,ncol(cf.exit)])
      h.exit <- pred.weibull(all$los,mu.exit,sigma.exit,what="hazard")
      
      # Calculate probability of dying at the end of hospital stay (forced<=1)
      all$pdead <- pmin(h.dead/h.exit,1)
      
      # Restrict ICU admission
      adp <- unlist(mapply(radp,n=1,madp=madp,vadp=vadp,SIMPLIFY=FALSE)) # simulated admission probability on each day
      restrict <- which(adp<1) # index of days for which a restriction on ICU admission should be applied
      if(length(restrict)>0){
        for(k in restrict){
          sel <- which(all$icu.in==days[k]); nsel <- length(sel)  # select patients that are supposed to enter ICU on day k
          if(nsel>0){
            refused <- rbinom(nsel,size=1,prob=1-adp[k])          # patient-specific binary indicator for effective ICU refusal on day k
            
            sel.refused <- sel[refused==1]                        # select patients which are refused in ICU
            all$hos.out[sel.refused] <- all$icu.in[sel.refused]   # set date of hospital exit on the theoretical date of ICU admission
            all$los[sel.refused] <- all$lag[sel.refused]          # correct LOS
            all$pdead[sel.refused] <- 1                           # set probability of dying to 1 for patients refused in ICU
          }
        }
      }
      
      # Generate death indicator
      all$dead <- sapply(all$pdead,rbinom,n=1,size=1)
    }
    
    # Calculate daily nb of deaths
    ndead <- sapply(days,function(dd){sum(all$dead[all$hos.out==dd])})

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

# -----------------------------------------------------------------------------
# Find best fitting FP power for continuous variable defined in xform
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
  grid <- cbind(pgrid,LL.grid)
  colnames(grid) <- c(paste0("p",c(1:degree)),"loglik")
  list(model=model,power=power,loglik=LL,aic=aic,grid=grid)
}

# -----------------------------------------------------------------------------
# AIC for linear model with one continuous variable modeled as fractional polynomials
aic <- function(model,fp.degree=0){
  LL <- logLik(model)[1]
  npar <- length(coef(model))+fp.degree
  -2*LL+2*npar
}

# -----------------------------------------------------------------------------
# Extract right hand side of model formula
xform <- function(formula){
  as.formula(paste0("~",as.character(formula)[3]),env=environment(formula))
}

# -----------------------------------------------------------------------------
# Predict survival and hazard from a Weibull fit
pred.weibull <- function(
  t,
  mu,
  sigma,
  what="survival"
){
  t[t==0] <- 0.001
  if(what=="survival"){output <- exp(-exp((log(t)-mu)/sigma))}
  #if(what=="survival"){output <- 1-pweibull(t,shape=1/sigma,scale=exp(mu))}
  if(what=="hazard"){output <- exp((log(t)-mu)/sigma)/(sigma*t)}
  output
}

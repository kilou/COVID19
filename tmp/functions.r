########################################################################################################
# NECESSARY FUNCTIONS FOR COVID-19 FORECASTS                                                           #
########################################################################################################
# ------------------------------------------------------------------------------------------------------
# Convert dates
conv <- function(x,format="%d.%m.%Y"){as.Date(strptime(as.character(x),format=format))}

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
# Random sample for proportion of incident case requiring intensive cares
rpic <- function(
  n,    # nb draws
  mpic, # median proportion
  vpic  # variability
){expit(rnorm(n,logit(mpic),sqrt(vpic)))}

# ------------------------------------------------------------------------------------------------------
# Random sample for lag (between COVID-19 test and ICU admission)
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
# Histogram for parameters
histo <- function(x,prob){
  par(mar=c(3,3,3,3),mgp=c(1.8,0.6,0))
  h <- hist(x,breaks=50,xlab="",ylab="",main="",col="#428bca",yaxt="n",yaxs="i",freq=FALSE)
  if(!is.null(prob)){
    stopifnot(length(prob)==3)
    q <- quantile(x,probs=prob)
    abline(v=q[c(1,3)],lty=2,lwd=2,col="red")
    abline(v=q[2],lwd=2,col="red")
    mtext(paste0(100*prob,"%"),side=3,at=q,col="red",cex=0.8)
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
  if(what=="ntot"){
    tit <- "Cumulative counts of confirmed cases"
    ylb <- "Counts"
    y <- data$ntot
  }
  
  par(mar=c(3,5,3,3),mgp=c(1.8,0.6,0))
  plot(range(days),range(y,Q),type="n",xlab="",ylab="",las=1)
  mtext(ylb,side=2,line=3)
  title(tit)
  polygon(x=c(days[past],rev(days[past])),y=c(Q[past,1],rev(Q[past,3])),col="lightgrey",border=NA)
  lines(days[past],Q[past,2],lwd=2,col="darkgrey")
  polygon(x=c(days[futur],rev(days[futur])),y=c(Q[futur,1],rev(Q[futur,3])),col="#428bca",border=NA)
  lines(days[futur],Q[futur,2],lwd=2,col="red")
  points(data$date,y,pch=19,col="black")
  points(days[futur[-1]],Q[futur[-1],2],pch=19,col="red")
  abline(v=today,lty=2)
  legend("topleft",legend=c("Observed counts","Predicted counts"),pch=c(19,19),col=c("black","red"),bty="n",cex=0.8)
  mtext(paste0(100*prob,"%"),side=4,at=Q[nrow(Q),],cex=0.8,las=1,col=c("#428bca","red","#428bca"),line=0.25)
}

# ------------------------------------------------------------------------------------------------------
# Forecast nb of ICU beds
pred.covid <- function(
  nday,     # nb of days to forcast
  nsim,     # nb of simulations
  pars,     # dataframe with parameters
  data,     # dataframe with VD data
  ncpu,     # nb of parallel processes (use 1 for serial compiutations)
  seed=1234 # seed for reproducible computations
){
  set.seed(seed)
  
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
  
  # Fill-in observed cumulative counts
  ntot <- matrix(nrow=nsim,ncol=j+nday)
  ntot[,1:j] <- t(data$ntot)[rep(1,nsim),] 
  
  # Predict future cumulative counts
  for(k in (j+1):(j+nday)){ntot[,k] <- round(ntot[,k-1]*rlam(nsim,mlam[k],vlam[k]))}
  
  # Calculate incident cases
  ninc <- matrix(nrow=nsim,ncol=j+nday)
  ninc[,1] <- ntot[,1]
  for(k in 1:(j+nday-1)){ninc[,k+1] <- ntot[,k+1]-ntot[,k]}
  if(sum(ninc<0)>0){stop("Some incident counts are negative!")}
  
  # Calculate incident cases that will require intensive cares at some point
  nicu <- matrix(nrow=nsim,ncol=j+nday)
  for(k in 1:(j+nday)){nicu[,k] <- round(rpic(nsim,mpic[k],vpic[k])*ninc[,k])}
  
  # Calculate nb of ICU beds required (function to be parallelized)
  fun <- function(s){
    npat <- nicu[s,]
    tst_i <- unlist(mapply(rep,x=1:(j+nday),times=npat,SIMPLIFY=FALSE))     # define COVID-19 test day for all patients that will be admitted in ICU
    lag_i <- unlist(mapply(rlag,n=npat,mlag=mlag,vlag=vlag,SIMPLIFY=FALSE)) # lag for all patients that will be admitted in ICU
    los_i <- unlist(mapply(rlos,n=npat,mlos=mlos,vlos=vlos,SIMPLIFY=FALSE)) # length of stay for all patients that will be admitted in ICU
    
    # Define ICU day-in and ICU day-out for these patients
    day.in <- tst_i+lag_i
    day.out <- tst_i+lag_i+los_i-1
    
    # Fill-in occupancy matrix for new patients admitted in ICU over the next few days
    occ <- matrix(0,nrow=sum(npat),ncol=j+nday)
    for(i in 1:nrow(occ)){occ[i,which(c(1:(j+nday))%in%c(day.in[i]:day.out[i]))] <- 1}
    
    # Return nb of occupied beds
    apply(occ,2,sum)
  }
  
  # Run serial/parallel calculations
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
  colnames(ntot) <- colnames(ninc) <- colnames(nicu) <- colnames(nbed) <- format(days,format="%d.%m.%Y")
  list(
    ntot=ntot, # cumulative counts of confirmed cases
    ninc=ninc, # counts of incident cases
    nicu=nicu, # counts of incident cases that will require intensive cares
    nbed=nbed, # predicted nb of occupied ICU beds
    data=data  # return data (for plotting)
  )
}
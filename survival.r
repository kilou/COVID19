set.seed(13042020)
setwd("~/Documents/Work/COVID19")
library(readxl)
library(writexl)
library(survival)

# Load data
raw <- as.data.frame(read_xlsx("data/20.04.09 - Données REDCap hôpitaux anonymisés.xlsx",sheet=1))
age <- raw[,"age"]
sex <- factor(raw[,"sex"],levels=c("M","F"))
hos_in <- as.Date(raw[,"arrivee_hopital"],"%Y/%m/%d")
icu_in <- as.Date(raw[,"debut_soins_intensifs"],"%Y/%m/%d")
icu_out <- as.Date(raw[,"fin_soins_intensifs"],"%Y/%m/%d")
hos_out <- as.Date(raw[,"sortie_hopital"],"%Y/%m/%d")
dead <- raw[,"deces"]
exit <- (!is.na(hos_out))*1

# Age categories
hist(age)
quantile(age,c(0.25,0.5,0.75))
age.cat <- cut(age,breaks=c(min(age),70,85,max(age)),include.lowest=TRUE)
page <- table(age.cat)/nrow(raw) # proportion of patients in each age category
table(dead,age.cat)
table(exit,age.cat)
col.cat <- c("green","orange","red")

# Calculate total LOS
los <- as.numeric(hos_out-hos_in)
cens <- (is.na(hos_out))*1
los[cens==1] <- as.numeric(max(hos_in,na.rm=T)-hos_in[cens==1])
los[los==0] <- 0.001 # to avoid issues with los=0

# Time (for predictions)
Time <- c(0.001,1:60)

###############################################################################
# TIME TO DEATH MODEL

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
survreg.FP <- function(time,event,x,covar=NULL,data=NULL,dist="weibull"){
  if(class(time)=="formula"){time <- model.matrix(time,data)[,-1]}
  if(class(event)=="formula"){event <- model.matrix(event,data)[,-1]}
  if(class(x)=="formula"){x <- model.matrix(x,data)[,-1]}
  if(class(covar)=="formula"){covar <- model.matrix(covar,data)[,-1]}
  
  require(survival)
  pgrid <- c(-2,-1,-0.5,0,0.5,1,2,3)
  S <- Surv(time,event)
  
  # Linear fit
  form0 <- "S~FP(x,1)"
  if(!is.null(covar)){form0 <- paste0(form0,"+covar")}
  fm0 <- survreg(as.formula(form0),dist=dist)
  LL0 <- fm0$loglik[2]
  
  # 1st degree FP
  fm1.grid <- rep(list(NULL),length(pgrid))
  for(k in 1:length(pgrid)){
    form1 <- "S~FP(x,pgrid[k])"
    if(!is.null(covar)){form1 <- paste0(form1,"+covar")}
    fm1.grid[[k]] <- survreg(as.formula(form1),dist=dist)
  }
  LL1.grid <- sapply(fm1.grid,function(model){model$loglik[2]})
  best1 <- rev(order(LL1.grid))[1]
  fm1 <- fm1.grid[[best1]]
  LL1 <- LL1.grid[best1]
  
  # Test 1st degree FP versus linear
  stat <- 2*(LL1-LL0)
  df <- 1
  pval1 <- 1-pchisq(stat,df)
  if(pval1<0.05){
    # 2nd degree FP
    pgrid2 <- expand.grid(pgrid,pgrid)
    pgrid2 <- pgrid2[which(pgrid2[,2]>=pgrid2[,1]),]
    fm2.grid <- rep(list(NULL),nrow(pgrid2))
    for(k in 1:nrow(pgrid2)){
      form2 <- "S~FP(x,pgrid2[k,])"
      if(!is.null(covar)){form2 <- paste0(form2,"+covar")}
      fm2.grid[[k]] <- survreg(as.formula(form2),dist=dist)
    }
    LL2.grid <- sapply(fm2.grid,function(model){model$loglik[2]})
    best2 <- rev(order(LL2.grid))[1]
    fm2 <- fm2.grid[[best2]]
    LL2 <- LL2.grid[best2]
    
    # Test 1st degree FP versus linear
    stat <- 2*(LL2-LL1)
    df <- 2
    pval2 <- 1-pchisq(stat,df)
    if(pval2<0.05){
      fm <- fm2 # Best model is fm2
      power <- pgrid2[best2,]
      pval <- pval2
    } else {
      fm <- fm1 # Best model is fm1
      power <- pgrid[best1]
      pval <- pval1
    }
  } else {
    fm <- fm0 # Best model is fm0 (linear)
    power <- 1
    pval <- pval1
  }
  list(model=fm,power=power,p.value=pval)
}

dat <- data.frame(
  age=age,
  age.cat=age.cat,
  sex=sex,
  los=los,
  exit=exit,
  dead=dead
)

fit.death <- survreg.FP(time=~los,event=~dead,x=~age,covar=~sex,data=dat,dist="weibull")
fit.exit <- survreg.FP(time=~los,event=~exit,x=~age,covar=~sex,data=dat,dist="weibull")


# -----------------------------------------------------------------------------
# TIME TO DEATH

# # Linear age
# fm1.lin <- survreg(Surv(los,dead)~age,data=dat,dist="weibull")
# summary(fm1.lin)
# AIC(fm1.lin)

# Categorical age
fm1.cat <- survreg(Surv(los,dead)~age.cat,data=dat,dist="weibull")
summary(fm1.cat)
AIC(fm1.cat)

# Best model
fm1 <- fm1.cat
mu1 <- fm1$coef
sig1 <- fm1$scale

# Plot survival
par(mfrow=c(1,1),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
fit1 <- survfit(Surv(los,dead)~age.cat,data=dat)
plot(fit1,xlim=c(0,35),ylim=c(0.5,1),lty=2,conf.int=F,xlab="Time to death",ylab="Survival",col=col.cat)
for(k in 1:nlevels(age.cat)){
  #m1 <- mu1[1]+mu1[2]*age.mid[k]
  m1 <- mu1[1]+c(0,mu1[-1])[k]
  lines(Time, exp(-exp((log(Time)-m1)/sig1)),lty=1,lwd=2,col=col.cat[k])
}
legend("bottomleft",legend=paste("Age",levels(age.cat)),lwd=2,col=col.cat,bty="n")

# -----------------------------------------------------------------------------
# TIME TO EXIT (Sex should be added!)

# # Linear age
# fm2.lin <- survreg(Surv(los,exit)~age,data=dat,dist="weibull")
# summary(fm2.lin)
# AIC(fm2.lin)

# Categorical age
fm2.cat <- survreg(Surv(los,exit)~age.cat,data=dat,dist="weibull")
summary(fm2.cat)
AIC(fm2.cat)

# Best model
fm2 <- fm2.cat
mu2 <- fm2$coef
sig2 <- fm2$scale

# Plot fraction still in hospital
par(mfrow=c(1,1),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
fit2 <- survfit(Surv(los,exit)~age.cat,data=dat)
plot(fit2,xlim=c(0,45),ylim=c(0,1),lty=2,conf.int=F,xlab="Time to exit",ylab="Fraction still hospitalized",col=col.cat)
for(k in 1:nlevels(age.cat)){
  #m2 <- mu2[1]+mu2[2]*age.mid[k]
  m2 <- mu2[1]+c(0,mu2[-1])[k]
  lines(Time, exp(-exp((log(Time)-m2)/sig2)),lty=1,lwd=2,col=col.cat[k])
}
legend("bottomleft",legend=paste("Age",levels(age.cat)),lwd=2,col=col.cat,bty="n")

# -----------------------------------------------------------------------------
# SAVE MODEL PARAMETERS IN XLSX FILE
lev <- levels(age.cat)
lev <- gsub("[","",lev,fixed=TRUE)
lev <- gsub("(","",lev,fixed=TRUE)
lev <- gsub("]","",lev,fixed=TRUE)
acat <- do.call("rbind",strsplit(lev,split=","))
pars <- data.frame(
  amin=acat[,1],
  amax=acat[,2],
  prob=as.numeric(page),
  mu1=mu1[1]+c(0,mu1[-1]),
  sig1=sig1,
  mu2=mu2[1]+c(0,mu2[-1]),
  sig2=sig2
)
write_xlsx(pars,path="params_surv.xlsx")

# -----------------------------------------------------------------------------
# PROBABILITY TO DIE AT THE END OF HOSPITAL STAY
h1 <- h2 <- matrix(nrow=length(Time),ncol=nlevels(age.cat))
colnames(h1) <- colnames(h2) <- levels(age.cat)
for(k in 1:nlevels(age.cat)){
  m1 <- mu1[1]+c(0,mu1[-1])[k]
  m2 <- mu2[1]+c(0,mu2[-1])[k]
  h1[,k] <- exp((log(Time)-m1)/sig1)/(sig1*Time)
  h2[,k] <- exp((log(Time)-m2)/sig2)/(sig2*Time)
}

# Plot probability of death at the end of hospital stay
h <- h1/h2
plot(range(Time),range(0,h),type="n",xlab="LOS",ylab="Probability")
title("Probability of death at the end of hospital stay")
for(k in 1:nlevels(age.cat)){
  lines(Time,h[,k],col=col.cat[k],lwd=2)
}
legend("bottom",legend=paste("Age",levels(age.cat)),lwd=2,col=col.cat,horiz=TRUE,bty="n")

# -----------------------------------------------------------------------------
# SIMULATIONS
N <- 100000

# Rebuild age distribution
A <- t(rmultinom(N,size=1,prob=page))

# Generate LOS according to exit model
age.sim <- factor(rep(NA,N),levels=levels(age.cat))
los.sim <- h1.sim <- h2.sim <- numeric(N)
for(k in 1:nlevels(age.cat)){
  # Parameters for age category
  m1 <- mu1[1]+c(0,mu1[-1])[k]
  m2 <- mu2[1]+c(0,mu2[-1])[k]
  
  # Age vector
  pos <- which(A[,k]==1)
  age.sim[pos] <- levels(age.cat)[k]
  
  # Generate LOS
  los.sim[pos] <- floor(rweibull(length(pos),shape=1/sig2,scale=exp(m2)))

  # Generate instant hazard of death and exit
  t <- los.sim[pos]
  t[t==0] <- 0.001
  h1.sim[pos] <- exp((log(t)-m1)/sig1)/(sig1*t)
  h2.sim[pos] <- exp((log(t)-m2)/sig2)/(sig2*t)
}
p.sim <- h1.sim/h2.sim

# Simulate deaths and calculate global mortality
dead.sim <- sapply(p.sim,rbinom,n=1,size=1)
mean(dead.sim)



















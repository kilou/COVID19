set.seed(23042020)
setwd("~/Documents/Work/COVID19")
source("functions.r")
library(readxl)
library(writexl)
library(survival)

# Load data
raw <- as.data.frame(read_xlsx("data/20.04.17 - Données REDCap hôpitaux anonymisés.xlsx",sheet=1))
age <- raw[,"age"]
sex <- factor(raw[,"sex"],levels=c("M","F"))
hos_in <- as.Date(raw[,"arrivee_hopital"],"%Y/%m/%d")
icu_in <- as.Date(raw[,"debut_soins_intensifs"],"%Y/%m/%d")
icu_out <- as.Date(raw[,"fin_soins_intensifs"],"%Y/%m/%d")
hos_out <- as.Date(raw[,"sortie_hopital"],"%Y/%m/%d")
dead <- raw[,"deces"]
exit <- (!is.na(hos_out))*1

# Calculate total LOS
los <- as.numeric(hos_out-hos_in)
cens <- (is.na(hos_out))*1
los[cens==1] <- as.numeric(max(hos_in,na.rm=T)-hos_in[cens==1])
los[los==0] <- 0.001 # to avoid issues with los=0

# Distribution of age categories and proportion of females in each age category
age.breaks <- c(0,15,30,45,60,75,90,105)
ncat <- length(age.breaks)-1
agecat <- cut(age,breaks=age.breaks,include.lowest=TRUE)
page <- round(table(agecat)/nrow(raw),3); page[ncat] <- 1-sum(page[1:(ncat-1)])
pfem <- round(table(agecat[sex=="F"])/table(agecat),3)

# Plot histogram of age distribution for each sex
pop <- rpop(n=1e06,breaks=age.breaks,page,pfem)
xM <- pop$age[pop$sex=="M"]; attr(xM,"breaks") <- age.breaks
xF <- pop$age[pop$sex=="F"]; attr(xF,"breaks") <- age.breaks
par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
histo(xM); title("Males")
histo(xF); title("Females")

###############################################################################
# MODEL PROPORTION OF PATIENTS REQUIRING IC (BY AGE AND SEX)
pos0 <- which(is.na(icu_in) & !is.na(hos_out) & dead==0) # did not require IC and left hospital alive
pos1 <- which(!is.na(icu_in))                            # did require IC
pos2 <- c(
  which(is.na(icu_in) & is.na(hos_out)),            # still in hospital without IC (will be imputed: they may still need IC in the future)
  which(is.na(icu_in) & !is.na(hos_out) & dead==1)  # those that died in hospital without going in ICU (will be imputed: they may have needed ICU but died too early)
)

# Define IC indicator
ic <- rep(NA,nrow(raw))
ic[pos0] <- 0
ic[pos1] <- 1

# Initial model
fm <- glm(ic~age+sex,family="binomial")
AIC(fm)
best.FP(fm,xform=~age,degree=1)$aic
best.FP(fm,xform=~age,degree=2)$aic
best.FP(fm,xform=~age,degree=2)$power

# Best icp model
#******************************************************
formula.icp <- ic~FP(age,c(3,3),shift=1,scale=100)+sex
#******************************************************

fm <- glm(formula.icp,family="binomial")
X <- model.matrix(xform(formula.icp))

# Multiple imputations for IC status of hospitalized patients but not yet in ICU
niter <- 500         # nb iterations
M <- 20              # nb imputed datasets
nimp <- length(pos2) # nb of imputed data
npar <- ncol(X)      # nb parameters in logistic model
beta.trace <- matrix(nrow=niter,ncol=npar)
vcov.trace <- rep(list(NULL),niter)

icp.imp <- rep(0,nimp) # individual probability to require IC
for(i in 1:niter){
  beta.tmp <- matrix(nrow=M,ncol=npar)
  vcov.tmp <- rep(list(NULL),M)
  for(m in 1:M){
    ic[pos2] <- sapply(icp.imp,rbinom,n=1,size=1)
    fm.tmp <- update(fm)
    beta.tmp[m,] <- coef(fm.tmp)
    vcov.tmp[[m]] <- vcov(fm.tmp)
  }
  
  # Pooled estimates
  beta.trace[i,] <- apply(beta.tmp,2,mean)
  
  # Rubin rules
  Vw <- Reduce("+",vcov.tmp)/M
  Vb <- var(beta.tmp)
  vcov.trace[[i]] <- Vw+(1+1/M)*Vb
  
  # Update IC requirement probability for patients without IC status
  icp.imp <- expit(as.numeric(X[pos2,]%*%beta.trace[i,]))
}  

# Convergence plots
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(k in 1:npar){
  plot(beta.trace[,k],type="l")
}

# Average parameters over last 100 iterations
last <- 100
beta.last <- beta.trace[(niter-last+1):niter,]
vcov.last <- vcov.trace[(niter-last+1):niter]
beta.icp <- apply(beta.last,2,mean)
Vw <- Reduce("+",vcov.last)/last
Vb <- var(beta.last)
V.icp <- Vw+(1+1/last)*Vb
names(beta.icp) <- colnames(V.icp)

# Plot model predictions
agepred <- c(0:max(age))
datM <- data.frame(age=agepred,sex=factor("M",levels=c("M","F")))
datF <- data.frame(age=agepred,sex=factor("F",levels=c("M","F")))
XpredM <- model.matrix(xform(formula.icp),datM)
XpredF <- model.matrix(xform(formula.icp),datF)

beta.sim <- MASS::mvrnorm(1000,beta.icp,V.icp)
icpM.sim <- t(apply(beta.sim,1,function(b){expit(as.numeric(XpredM%*%b))}))
icpF.sim <- t(apply(beta.sim,1,function(b){expit(as.numeric(XpredF%*%b))}))
icpM <- t(apply(icpM.sim,2,quantile,prob=c(0.025,0.5,0.975)))
icpF <- t(apply(icpF.sim,2,quantile,prob=c(0.025,0.5,0.975)))

pdf("surv/icp.pdf",width=12,height=8)
par(mfrow=c(1,1),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
plot(range(agepred),range(icpM,icpF),type="n",xlab="Age",ylab="Proportion of hospitalized patients requiring IC")
polygon(x=c(agepred,rev(agepred)),y=c(icpM[,1],rev(icpM[,3])),border=NA,col=rgb(t(col2rgb("blue"))/255,alpha=0.3))
polygon(x=c(agepred,rev(agepred)),y=c(icpF[,1],rev(icpF[,3])),border=NA,col=rgb(t(col2rgb("red"))/255,alpha=0.3))
lines(agepred,icpM[,2],col="blue",lwd=2)
lines(agepred,icpF[,2],col="red",lwd=2)
legend("topleft",c("Males","Females"),col=c("blue","red"),lwd=2,bty="n")
dev.off()

# Create imputed datasets with agecat,sex ic status (imputed), los, exit and dead
imp <- rep(list(data.frame(age=age,sex=sex,ic=ic,los=los,exit=exit,dead=dead)),M)
beta.sim <- MASS::mvrnorm(M,beta.icp,V.icp)
for(m in 1:M){
  p <- expit(as.numeric(X[pos2,]%*%beta.sim[m,]))
  imp[[m]]$ic[pos2] <- sapply(p,rbinom,n=1,size=1)
}

# Proportion of patients requiring IC
mean(sapply(imp,function(x){mean(x$ic)}))
mean(sapply(imp,function(x){mean(x$ic[x$sex=="M"])}))
mean(sapply(imp,function(x){mean(x$ic[x$sex=="F"])}))

# Biased estimates
mean(!is.na(icu_in))
mean(!is.na(icu_in[sex=="M"]))
mean(!is.na(icu_in[sex=="F"]))

# Time (for predictions)
Time <- c(0.001,1:60)
npred <- length(Time)

# Age categories for mortality plots
agecat2 <- cut(age,breaks=c(0,70,85,Inf),include.lowest=T)
for(m in 1:M){imp[[m]] <- cbind(imp[[m]],agecat2)}
age.mid <- tapply(age,agecat2,mean)
col.age <- c("green","orange","red")

###############################################################################
# TIME TO DEATH MODEL

# Base model
fm10 <- survreg(Surv(los,dead)~age+ic+sex,data=imp[[1]],dist="weibull"); aic(fm10)

# Test interaction age*ic using degree 2 FP
best.FP(fm10,xform=~age,degree=2)$power
fm11 <- survreg(Surv(los,dead)~FP(age,c(3,3))+ic+sex,data=imp[[1]],dist="weibull"); aic(fm11,2) # worse than fm10
fm12 <- survreg(Surv(los,dead)~FP(age,c(3,3))*ic+sex,data=imp[[1]],dist="weibull"); aic(fm12,2) # worse than fm10

# Test interaction age*ic using degree 1 FP
best.FP(fm10,xform=~age,degree=1)$power
fm13 <- survreg(Surv(los,dead)~FP(age,0.5)+ic+sex,data=imp[[1]],dist="weibull"); aic(fm13,1) # worse than fm10
fm14 <- survreg(Surv(los,dead)~FP(age,0.5)*ic+sex,data=imp[[1]],dist="weibull"); aic(fm14,1) # worse than fm10

# Best death model
#****************************************
formula.dead <- Surv(los,dead)~age+ic+sex
#****************************************

# Fit best model to multiple imputed datasets
fm1 <- list()
for(m in 1:M){
  fm1[[m]] <- survreg(formula.dead,data=imp[[m]],dist="weibull")
}
beta1.tmp <- t(sapply(fm1,function(x){c(coef(x),log(x$scale))})) 
colnames(beta1.tmp)[ncol(beta1.tmp)] <- "log(scale)"
beta.dead <- apply(beta1.tmp,2,mean)
Vw1 <- Reduce("+",lapply(fm1,vcov))/M
Vb1 <- var(beta1.tmp)
V.dead <- Vw1+(1+1/M)*Vb1
se.dead <- sqrt(diag(V.dead))
pval.dead <- 2*pnorm(abs(beta.dead/se.dead),lower.tail=F)

# Plot survival
pdf("surv/death.pdf",width=12,height=8)
form <- as.formula(paste0("t~",as.character(xform(formula.dead))[2]),env=environment(formula.dead))
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(h in 1:2){ # ic status
  for(l in 1:2){ # sex
    plot(c(0,35),c(0,1),type="n",xlab="Time",ylab="Survival")
    title(paste(c("Without IC","With IC")[h],"-",c("Males","Females")[l]))
    for(m in 1:M){
      km1 <- survfit(Surv(los,dead)~agecat2,data=subset(imp[[m]],ic==c(0,1)[h] & sex==c("M","F")[l]))
      lines(km1,col=col.age,lty=3)
    }
    for(k in 1:length(age.mid)){
      newdat <- data.frame(t=Time,age=rep(age.mid[k],npred),sex=factor(c("M","F")[l],levels=c("M","F")),ic=c(0,1)[h])
      surv <- pred.weibull(form,newdat,beta.dead,what="survival")
      lines(Time,surv,lwd=2,col=col.age[k])
    }
    if(h==1 & l==1){legend("bottomleft",levels(agecat2),col=col.age,lwd=1,bty="n")}
  }
}
dev.off()
 
###############################################################################
# TIME TO EXIT

# Base model
fm20 <- survreg(Surv(los,exit)~age+ic+sex,data=imp[[1]],dist="weibull"); aic(fm20)

# Test interaction age*ic using degree 2 FP
best.FP(fm20,xform=~age,degree=2)$power
fm21 <- survreg(Surv(los,exit)~FP(age,c(1,1))+ic+sex,data=imp[[1]],dist="weibull"); aic(fm21,2) # worse than fm20
fm22 <- survreg(Surv(los,exit)~FP(age,c(1,1))*ic+sex,data=imp[[1]],dist="weibull"); aic(fm22,2) # better than fm20

# Test interaction sex*ic
fm23 <- survreg(Surv(los,exit)~FP(age,c(1,1))*ic+sex*ic,data=imp[[1]],dist="weibull"); aic(fm23,2) # worse than fm22

# Test interaction sex*age
fm24 <- survreg(Surv(los,exit)~FP(age,c(1,1))*ic+sex*FP(age,c(1,1)),data=imp[[1]],dist="weibull"); aic(fm24,2) # worse than fm22

# Best exit model
#*********************************************************************
formula.exit <- Surv(los,exit)~FP(age,c(1,1),shift=1,scale=100)*ic+sex
#*********************************************************************

# Fit best model to multiple imputed datasets
fm2 <- list()
for(m in 1:M){
  fm2[[m]] <- survreg(formula.exit,data=imp[[m]],dist="weibull")
}
beta2.tmp <- t(sapply(fm2,function(x){c(coef(x),log(x$scale))})) 
colnames(beta2.tmp)[ncol(beta2.tmp)] <- "log(scale)"
beta.exit <- apply(beta2.tmp,2,mean)
Vw2 <- Reduce("+",lapply(fm2,vcov))/M
Vb2 <- var(beta2.tmp)
V.exit <- Vw2+(1+1/M)*Vb2
se.exit <- sqrt(diag(V.exit))
pval.exit <- 2*pnorm(abs(beta.exit/se.exit),lower.tail=F)

# Plot fraction of patients still hospitalized
pdf("surv/exit.pdf",width=12,height=8)
form <- as.formula(paste0("t~",as.character(xform(formula.exit))[2]),env=environment(formula.exit))
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(h in 1:2){ # ic status
  for(l in 1:2){ # sex
    plot(c(0,35),c(0,1),type="n",xlab="Time",ylab="Fraction still hospitalized")
    title(paste(c("Without IC","With IC")[h],"-",c("Males","Females")[l]))
    for(m in 1:M){
      km1 <- survfit(Surv(los,exit)~agecat2,data=subset(imp[[m]],ic==c(0,1)[h] & sex==c("M","F")[l]))
      lines(km1,col=col.age,lty=3)
    }
    for(k in 1:length(age.mid)){
      newdat <- data.frame(t=Time,age=rep(age.mid[k],npred),sex=factor(c("M","F")[l],levels=c("M","F")),ic=c(0,1)[h])
      surv <- pred.weibull(form,newdat,beta.exit,what="survival")
      lines(Time,surv,lwd=2,col=col.age[k])
    }
    if(h==1 & l==1){legend("topright",levels(agecat),col=col.age,lwd=1,bty="n")}
  }
}
dev.off()

###############################################################################
# SAVE MORTALITY MODELS FORMULA AND COEFFICIENTS
m_models <- list(
  icp=list(
    formula=formula.icp,
    coef=beta.icp,
    vcov=V.icp
  ),
  dead=list(
    formula=formula.dead,
    coef=beta.dead,
    vcov=V.dead
  ),
  exit=list(
    formula=formula.exit,
    coef=beta.exit,
    vcov=V.exit
  )
)
save(m_models,file="m_models.Rdata")

# -----------------------------------------------------------------------------
# PROBABILITY TO DIE AT THE END OF HOSPITAL STAY
# load("m_models.Rdata")
# 
# pop <- rpop(n=10000,breaks=age.breaks,page,pfem)
# 
# 
# h1 <- h2 <- matrix(nrow=length(Time),ncol=nlevels(age.cat))
# colnames(h1) <- colnames(h2) <- levels(age.cat)
# for(k in 1:nlevels(age.cat)){
#   m1 <- mu1[1]+c(0,mu1[-1])[k]
#   m2 <- mu2[1]+c(0,mu2[-1])[k]
#   h1[,k] <- exp((log(Time)-m1)/sig1)/(sig1*Time)
#   h2[,k] <- exp((log(Time)-m2)/sig2)/(sig2*Time)
# }
# 
# # Plot probability of death at the end of hospital stay
# h <- h1/h2
# plot(range(Time),range(0,h),type="n",xlab="LOS",ylab="Probability")
# title("Probability of death at the end of hospital stay")
# for(k in 1:nlevels(age.cat)){
#   lines(Time,h[,k],col=col.cat[k],lwd=2)
# }
# legend("bottom",legend=paste("Age",levels(age.cat)),lwd=2,col=col.cat,horiz=TRUE,bty="n")

# -----------------------------------------------------------------------------
# SIMULATIONS
load("m_models.Rdata")

# Draw age and sex in population
pop <- rpop(n=10000,breaks=age.breaks,page,pfem)

# Generate IC status
X <- model.matrix(xform(m_models$icp$formula),pop)
p <- expit(as.numeric(X%*%m_models$icp$coef))
pop$ic <- sapply(p,rbinom,n=1,size=1)

# Generate LOS according to exit model
X <- model.matrix(xform(m_models$exit$formula),pop)
cf <- m_models$exit$coef; ncf <- length(cf)
beta <- cf[1:(ncf-1)]
sigma <- exp(cf[ncf])
mu <- as.numeric(X%*%beta)
pop$los <- sapply(exp(mu),rweibull,n=1,shape=1/sigma)

f.dead <- as.formula(paste0("los~",as.character(xform(m_models$dead$formula))[2]),env=environment(m_models$dead$formula))
f.exit <- as.formula(paste0("los~",as.character(xform(m_models$exit$formula))[2]),env=environment(m_models$exit$formula))

h1.sim <- pred.weibull(f.dead,pop,m_models$dead$coef,what="hazard")
h2.sim <- pred.weibull(f.exit,pop,m_models$exit$coef,what="hazard")
p.sim <- h1.sim/h2.sim

pos0 <- which(pop$ic==0)
pos1 <- which(pop$ic==1)

par(mfrow=c(1,2))
plot(pop$age[pos0],p.sim[pos0],col=c("blue","red")[(pop$sex=="F")*1+1][pos0])
abline(h=1,lty=2)

plot(pop$age[pos1],p.sim[pos1],col=c("blue","red")[(pop$sex=="F")*1+1][pos1])
abline(h=1,lty=2)





dead.sim <- sapply(p.sim,rbinom,n=1,size=1)
mean(dead.sim,na.rm=T)




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




















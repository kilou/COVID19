set.seed(23042020)
setwd("~/Documents/Work/COVID19")
source("functions.r")
library(readxl)
library(writexl)
library(survival)

# Time (for predictions)
Time <- c(0.001,1:60)
npred <- length(Time)

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

# Calculate lag
lag <- as.numeric(icu_in-hos_in)
fit.lag <- fit.nb(lag)

# Calculate total LOS
los <- as.numeric(hos_out-hos_in)
cens <- (is.na(hos_out))*1
los[cens==1] <- as.numeric(max(hos_in,na.rm=T)-hos_in[cens==1])
los[los==0] <- 0.001 # to avoid issues with los=0

# Distribution of age categories and proportion of females in each age category
age.breaks <- c(0,64,74,104)
ncat <- length(age.breaks)-1
agecat <- cut(age,breaks=age.breaks,include.lowest=TRUE)
page <- round(table(agecat)/nrow(raw),3); page[ncat] <- 1-sum(page[1:(ncat-1)])
pfem <- round(table(agecat[sex=="F"])/table(agecat),3)

# Plot histogram of age distribution for each sex
pop <- rpop(n=1e06,breaks=age.breaks,page,pfem)
# xM <- pop$age[pop$sex=="M"]; attr(xM,"breaks") <- age.breaks
# xF <- pop$age[pop$sex=="F"]; attr(xF,"breaks") <- age.breaks
# par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
# histo(xM); title("Males")
# histo(xF); title("Females")

###############################################################################
# MODEL PROPORTION OF PATIENTS REQUIRING IC (BY AGE AND SEX)
pos0 <- which(is.na(icu_in) & !is.na(hos_out)) # did not require IC and left hospital
pos1 <- which(!is.na(icu_in))                  # did require IC
pos2 <- which(is.na(icu_in) & is.na(hos_out))  # still in hospital without IC (will be imputed: they may still need IC in the future)
  
# Define IC indicator
ic <- rep(NA,nrow(raw))
ic[pos0] <- 0
ic[pos1] <- 1

# Initial model
fm20 <- glm(ic~cut(age,breaks=c(0,64,74,104),include.lowest=T)+sex,family="binomial"); aic(fm20)
fm21 <- glm(ic~cut(age,breaks=c(0,64,74,104),include.lowest=T)*sex,family="binomial"); aic(fm21)
summary(fm20)

# Best icp model
#******************************************************
formula.icp <- ic~cut(age,breaks=c(0,64,74,104),include.lowest=T)+sex
#******************************************************
col.icp <- c("green","orange","red")
mid.icp <- tapply(age,cut(age,breaks=c(0,64,74,104),include.lowest=T),mean)

# Fit icp model
fm.icp <- glm(formula.icp,family="binomial")
coef.icp <- coef(fm.icp)
V.icp <- vcov(fm.icp)

# Plot model predictions
agepred <- c(0:max(age))
datM <- data.frame(age=agepred,sex=factor("M",levels=c("M","F")))
datF <- data.frame(age=agepred,sex=factor("F",levels=c("M","F")))
XpredM <- model.matrix(xform(formula.icp),datM)
XpredF <- model.matrix(xform(formula.icp),datF)

beta.sim <- MASS::mvrnorm(1000,coef.icp,V.icp)
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

# Create imputed datasets with IC status
M <- 20
X <- model.matrix(xform(formula.icp),data.frame(age=age,sex=sex))
beta.sim <- MASS::mvrnorm(M,coef.icp,V.icp)
imp <- rep(list(data.frame(age=age,sex=sex,ic=ic,los=los,exit=exit,dead=dead)),M)
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

###############################################################################
# TIME TO DEATH MODEL

# Base model
fm30 <- survreg(Surv(los,dead)~cut(age,breaks=c(0,64,74,104),include.lowest=T)+ic+sex,data=imp[[1]],dist="weibull"); aic(fm30)
fm31 <- survreg(Surv(los,dead)~cut(age,breaks=c(0,64,74,104),include.lowest=T)*ic+sex,data=imp[[1]],dist="weibull"); aic(fm31)
fm32 <- survreg(Surv(los,dead)~cut(age,breaks=c(0,64,74,104),include.lowest=T)*ic*sex,data=imp[[1]],dist="weibull"); aic(fm32)
fm33 <- survreg(Surv(los,dead)~cut(age,breaks=c(0,64,74,104),include.lowest=T),data=imp[[1]],dist="weibull"); aic(fm33)

# # Test interaction age*ic using degree 2 FP
# best.FP(fm10,xform=~age,degree=2)$power
# fm11 <- survreg(Surv(los,dead)~FP(age,c(3,3))+ic+sex,data=imp[[1]],dist="weibull"); aic(fm11,2) # worse than fm10
# fm12 <- survreg(Surv(los,dead)~FP(age,c(3,3))*ic+sex,data=imp[[1]],dist="weibull"); aic(fm12,2) # worse than fm10
# 
# # Test interaction age*ic using degree 1 FP
# best.FP(fm10,xform=~age,degree=1)$power
# fm13 <- survreg(Surv(los,dead)~FP(age,0.5)+ic+sex,data=imp[[1]],dist="weibull"); aic(fm13,1) # worse than fm10
# fm14 <- survreg(Surv(los,dead)~FP(age,0.5)*ic+sex,data=imp[[1]],dist="weibull"); aic(fm14,1) # worse than fm10
# 
# fm15 <- survreg(Surv(los,dead)~FP(age,c(1,1),shift=1,scale=100)*ic+sex,data=imp[[1]],dist="weibull"); aic(fm15)

# Best death model
#****************************************
formula.dead <- Surv(los,dead)~cut(age,breaks=c(0,64,74,104),include.lowest=T)
#****************************************
col.dead <- c("green","orange","red")
mid.dead <- tapply(age,cut(age,breaks=c(0,64,74,104),include.lowest=T),mean)

# Fit best model to multiple imputed datasets
fm.dead <- list()
for(m in 1:M){
  fm.dead[[m]] <- survreg(formula.dead,data=imp[[m]],dist="weibull")
}
cf.tmp <- t(sapply(fm.dead,function(x){c(coef(x),log(x$scale))})) 
colnames(cf.tmp)[ncol(cf.tmp)] <- "log(scale)"
coef.dead <- apply(cf.tmp,2,mean)
Vw <- Reduce("+",lapply(fm.dead,vcov))/M
Vb <- var(cf.tmp)
V.dead <- Vw+(1+1/M)*Vb
se.dead <- sqrt(diag(V.dead))
pval.dead <- 2*pnorm(abs(coef.dead/se.dead),lower.tail=F)

# Plot survival
pdf("surv/death.pdf",width=12,height=8)
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(h in 1:2){ # ic status
  for(l in 1:2){ # sex
    plot(c(0,35),c(0,1),type="n",xlab="Time",ylab="Survival")
    title(paste(c("Without IC","With IC")[h],"-",c("Males","Females")[l]))
    for(m in 1:M){
      km <- survfit(Surv(los,dead)~cut(age,breaks=c(0,64,74,104)),data=subset(imp[[m]],ic==c(0,1)[h] & sex==c("M","F")[l]))
      lines(km,col=col.dead,lty=3)
    }
    for(k in 1:length(mid.dead)){
      newdat <- data.frame(age=rep(mid.dead[k],npred),sex=factor(c("M","F")[l],levels=c("M","F")),ic=c(0,1)[h])
      X <- model.matrix(xform(formula.dead),newdat)
      cf <- coef.dead
      mu <- as.numeric(X%*%cf[-length(cf)])
      sigma <- exp(cf[length(cf)])
      surv <- pred.weibull(Time,mu,sigma,what="survival")
      lines(Time,surv,lwd=2,col=col.dead[k])
    }
    #if(h==1 & l==1){legend("bottomleft",levels(agecat2),col=col.age,lwd=1,bty="n")}
  }
}
dev.off()
 
###############################################################################
# TIME TO EXIT

# Base model
fm40 <- survreg(Surv(los,exit)~cut(age,breaks=c(0,64,74,104),include.lowest=T)+ic+sex,data=imp[[1]],dist="weibull"); aic(fm40)
fm41 <- survreg(Surv(los,exit)~cut(age,breaks=c(0,64,74,104),include.lowest=T)*ic+sex,data=imp[[1]],dist="weibull"); aic(fm41)
fm42 <- survreg(Surv(los,exit)~cut(age,breaks=c(0,64,74,104),include.lowest=T)*ic*sex,data=imp[[1]],dist="weibull"); aic(fm42)

# # Test interaction age*ic using degree 2 FP
# best.FP(fm20,xform=~age,degree=2)$power
# fm21 <- survreg(Surv(los,exit)~FP(age,c(3,3))+ic+sex,data=imp[[1]],dist="weibull"); aic(fm21,2) # worse than fm20
# fm22 <- survreg(Surv(los,exit)~FP(age,c(3,3))*ic+sex,data=imp[[1]],dist="weibull"); aic(fm22,2) # slightly better than fm20
# 
# # Test interaction sex*ic
# fm23 <- survreg(Surv(los,exit)~FP(age,c(3,3))*ic+sex*ic,data=imp[[1]],dist="weibull"); aic(fm23,2) # worse than fm22
# 
# # Test interaction sex*age
# fm24 <- survreg(Surv(los,exit)~FP(age,c(3,3))*ic+sex*FP(age,c(1,1)),data=imp[[1]],dist="weibull"); aic(fm24,2) # worse than fm22
# 
# fm25 <- survreg(Surv(los,exit)~age*ic+sex,data=imp[[1]],dist="weibull"); aic(fm25,2) # worse than fm22
# best.FP(fm25,xform=~age,degree=2)$power

# Best exit model
#*********************************************************************
formula.exit <- Surv(los,exit)~cut(age,breaks=c(0,64,74,104),include.lowest=T)*ic*sex
#formula.exit <- Surv(los,exit)~FP(age,c(3,3),shift=1,scale=100)*(ic+sex)+ic*sex
#*********************************************************************
col.exit <- c("green","orange","red")
mid.exit <- tapply(age,cut(age,breaks=c(0,64,74,104),include.lowest=T),mean)

# Fit best model to multiple imputed datasets
fm.exit <- list()
for(m in 1:M){
  fm.exit[[m]] <- survreg(formula.exit,data=imp[[m]],dist="weibull")
}
cf.tmp <- t(sapply(fm.exit,function(x){c(coef(x),log(x$scale))})) 
colnames(cf.tmp)[ncol(cf.tmp)] <- "log(scale)"
coef.exit <- apply(cf.tmp,2,mean)
Vw <- Reduce("+",lapply(fm.exit,vcov))/M
Vb <- var(cf.tmp)
V.exit <- Vw+(1+1/M)*Vb
se.exit <- sqrt(diag(V.exit))
pval.exit <- 2*pnorm(abs(coef.exit/se.exit),lower.tail=F)

# Plot fraction of patients still hospitalized
pdf("surv/exit.pdf",width=12,height=8)
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(h in 1:2){ # ic status
  for(l in 1:2){ # sex
    plot(c(0,60),c(0,1),type="n",xlab="Time",ylab="Fraction still hospitalized")
    title(paste(c("Without IC","With IC")[h],"-",c("Males","Females")[l]))
    for(m in 1:M){
      km <- survfit(Surv(los,exit)~cut(age,breaks=c(0,64,74,104),include.lowest=T),data=subset(imp[[m]],ic==c(0,1)[h] & sex==c("M","F")[l]))
      lines(km,col=col.exit,lty=3)
    }
    for(k in 1:length(mid.exit)){
      newdat <- data.frame(age=rep(mid.exit[k],npred),sex=factor(c("M","F")[l],levels=c("M","F")),ic=c(0,1)[h])
      X <- model.matrix(xform(formula.exit),newdat)
      cf <- coef.exit
      mu <- as.numeric(X%*%cf[-length(cf)])
      sigma <- exp(cf[length(cf)])
      surv <- pred.weibull(Time,mu,sigma,what="survival")
      lines(Time,surv,lwd=2,col=col.exit[k])
    }
    #if(h==1 & l==1){legend("topright",levels(agecat2),col=col.age,lwd=1,bty="n")}
  }
}
dev.off()

###############################################################################
# SAVE MORTALITY MODELS FORMULA AND COEFFICIENTS
mort <- list(
  lag=list(
    mlag=fit.lag$mean,
    vlag=fit.lag$variance
  ),
  icp=list(
    formula=formula.icp,
    coef=coef.icp,
    vcov=V.icp
  ),
  dead=list(
    formula=formula.dead,
    coef=coef.dead,
    vcov=V.dead
  ),
  exit=list(
    formula=formula.exit,
    coef=coef.exit,
    vcov=V.exit
  )
)
save(mort,file="mort.Rdata")



# Plot two survival curves
pdf("surv/compare.pdf",width=12,height=8)
par(mfrow=c(2,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
for(h in 1:2){ # ic status
  for(l in 1:2){ # sex
    plot(c(0,60),c(0,1),type="n",xlab="Time",ylab="")
    title(paste(c("Without IC","With IC")[h],"-",c("Males","Females")[l]))
    for(k in 1:length(age.mid)){
      newdat <- data.frame(age=rep(age.mid[k],npred),sex=factor(c("M","F")[l],levels=c("M","F")),ic=c(0,1)[h])
      
      X1 <- model.matrix(xform(formula.dead),newdat)
      cf1 <- beta.dead
      mu.dead <- as.numeric(X1%*%cf1[-length(cf1)])
      sigma.dead <- exp(cf1[length(cf1)])
      surv.dead <- pred.weibull(Time,mu.dead,sigma.dead,what="survival")
      
      X2 <- model.matrix(xform(formula.exit),newdat)
      cf2 <- beta.exit
      mu.exit <- as.numeric(X2%*%cf2[-length(cf2)])
      sigma.exit <- exp(cf2[length(cf2)])
      surv.exit <- pred.weibull(Time,mu.exit,sigma.exit,what="survival")
      
      lines(Time,surv.dead,lwd=2,col=col.age[k])
      lines(Time,surv.exit,lty=2,col=col.age[k])
    }
    #if(h==1 & l==1){legend("topright",levels(agecat2),col=col.age,lwd=1,bty="n")}
  }
}
dev.off()

# -----------------------------------------------------------------------------
# PROBABILITY TO DIE AT THE END OF HOSPITAL STAY
# load("mort.Rdata")
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
load("mort.Rdata")

# Draw age and sex in population
pop <- rpop(n=10000,breaks=age.breaks,page,pfem)

# Generate IC status
X <- model.matrix(xform(mort$icp$formula),pop)
p <- expit(as.numeric(X%*%mort$icp$coef))
pop$ic <- sapply(p,rbinom,n=1,size=1)

# Generate LOS according to exit model
X <- model.matrix(xform(mort$exit$formula),pop)
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

plot(h1.sim,h2.sim)
abline(0,1)



range(p.sim)

pos0 <- which(pop$ic==0)
pos1 <- which(pop$ic==1)

pdf("surv/psim.pdf",width=12,height=8)
par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
plot(pop$age[pos0],p.sim[pos0],col=c("blue","red")[(pop$sex=="F")*1+1][pos0],main="Without IC",xlim=c(0,105),ylim=c(0,2),xlab="Age",ylab="P(death)")
abline(h=1,lty=2)
legend("topleft",c("Males","Females"),col=c("blue","red"),pch=1,bty="n")

plot(pop$age[pos1],p.sim[pos1],col=c("blue","red")[(pop$sex=="F")*1+1][pos1],main="With IC",xlim=c(0,105),ylim=c(0,2),xlab="Age",ylab="P(death)")
abline(h=1,lty=2)
dev.off()

par(mfrow=c(1,2),mar=c(3,3,2,0.5),mgp=c(1.8,0.6,0))
plot(pop$los[pos0],p.sim[pos0],col=c("blue","red")[(pop$sex=="F")*1+1][pos0],main="Without IC",xlim=c(0,105),ylim=c(0,2),xlab="Age",ylab="P(death)")
abline(h=1,lty=2)
legend("topleft",c("Males","Females"),col=c("blue","red"),pch=1,bty="n")

plot(pop$los[pos1],p.sim[pos1],col=c("blue","red")[(pop$sex=="F")*1+1][pos1],main="With IC",xlim=c(0,105),ylim=c(0,2),xlab="Age",ylab="P(death)")
abline(h=1,lty=2)




par(mfrow=c(1,2))
pos <- which(p.sim>1)
plot(pop$age,h1.sim)
points(pop$age[pos],h1.sim[pos],col="red")
plot(pop$age,h2.sim,ylim=c(0,0.1))
points(pop$age[pos],h2.sim[pos],col="red")

plot(pop$age,pop$los)

boxplot(los~ic,data=pop)



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





















setwd("~/Documents/dataFiles_masters/")
source("Source Error Confidence/DBDA2Eprograms/DBDA2E-utilities.R")

#setwd("~/Johanny/")
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceIncorrect_balanced/collapsedStrength")

detectCores() #see how many cores you have
# Kruschke's utilities code sets best
#  default chain #'s and methods for
#  your machine. Variables that hold
#  those values:
nChainsDefault
runjagsMethodDefault


#logistic functions
logistic=function(logit) exp(logit)/(1+exp(logit))
logit=function(p) log(p/(1-p))

# Load the data:
#dat=read.csv("FromJohanny/strength order/SandersExp1_Strength_0.csv")
#dat=as.matrix(dat)

# dat1=read.csv("SandersExp1_collapseStrength.csv",
#               stringsAsFactors = F)
# dat1=as.matrix(dat1)
# 
# dat1c=read.csv("SandersExp1_SourceCorrect_CollapseStrength.csv",
#               stringsAsFactors = F)
# dat1c=as.matrix(dat1c)
# 
# dat2=read.csv("SandersExp2_collapseStrength.csv",
#               stringsAsFactors = F)
# dat2=as.matrix(dat2)
# 
# dat2c=read.csv("SandersExp2_SourceCorrect_CollapseStrength.csv",
#                stringsAsFactors = F)
# dat2c=as.matrix(dat2c)
# 
# dat=rbind(dat1)#,dat2)
# datc=rbind(dat1c)#,dat2c)

library("dplyr")
data_all <- list.files(path = getwd(), pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 
dat=as.matrix(data_all)

setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceCorrect_balanced/CollapsedStrength")
data_all_c <- list.files(path = getwd(), pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 
datc=as.matrix(data_all_c)


hh=as.numeric(dat[,'HC.hit.HC.error'])
hl=as.numeric(dat[,'HC.hit.LC.error'])
lh=as.numeric(dat[,'LC.hit.HC.error'])
ll=as.numeric(dat[,'LC.hit.LC.error'])

hhc=as.numeric(datc[,'HC.hit.HC.correct'])
hlc=as.numeric(datc[,'HC.hit.LC.correct'])
lhc=as.numeric(datc[,'LC.hit.HC.correct'])
llc=as.numeric(datc[,'LC.hit.LC.correct'])


htot=hh+hl
ltot=lh+ll

htotc=hhc+hlc
ltotc=lhc+llc

htotb=htot+htotc
ltotb=ltot+ltotc

sCorHIC = hhc+hlc
sCorLIC = lhc+llc

dataset=dat[,'Study']
datasetc=datc[,'Study']
#these should match. To check:
# cbind(dataset,datasetc)


dataList = list(    # Put the information into a list.
  sCorHIC = sCorHIC,
  sCorLIC = sCorLIC,
  hh = hh,
  lh = lh,
  hhc = hhc,
  lhc = lhc,
  htot = htot,
  ltot = ltot,
  htotc = htotc,
  ltotc = ltotc,
  htotb = htotb,
  ltotb = ltotb,
  dataset = dat[,'Study'],
  nDataset = max(dat[,'Study']),
  part = 1:nrow(dat) ,
  nPart = nrow(dat) 
)


# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    sCorHIC[i] ~ dbinom( thetaHIC[i], htotb[i] )
    sCorLIC[i] ~ dbinom( thetaLIC[i], ltotb[i] )

    thetaHIC[i] = 1-pnorm(0, dpHIC[i]/2, 1/ 1 ^2)
    thetaLIC[i] = 1-pnorm(0, dpLIC[i]/2, 1/ 1 ^2)
    
    dpHIC[i] = beta0[i] + .5*icEff[i] 
    dpLIC[i] = beta0[i] - .5*icEff[i] 
    
    beta0[i] ~ dnorm(dsMbeta0[dataset[i]] , 1/ Sbeta0  ^2)
    icEff[i] ~ dnorm(dsMicEff[dataset[i]], 1/ SicEff  ^2)
  }
  
  for(j in 1:nDataset){
    dsMbeta0[j] ~ dnorm(Mbeta0, 1/ SdsBeta0 ^2)
    dsMicEff[j] ~ dnorm(MicEff, 1/ SdsIcEff ^2)
  }
  
  Mbeta0 ~ dnorm( 0.5 , 1/ 1 ^2)T(0,)
  Sbeta0 ~ dnorm( .4 , 1/ .4 ^2)T(.1,2)
  MicEff ~ dnorm( 0 , 1/ 1 ^2)
  SicEff ~ dnorm( .4 , 1/ .2 ^2)T(.1,1)
  SdsBeta0 ~ dnorm( .4 , 1/ .4 ^2)T(.1,2)
  SdsIcEff ~ dnorm( .4 , 1/ .2 ^2)T(.1,1)
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )


# Run the chains:
runJagsOut = run.jags(
  method=runjagsMethodDefault, #"parallel" or "rjags" 
  model="TEMPmodel.txt",
  monitor=c("Mbeta0","dsMbeta0","dsMicEff","MicEff","Sbeta0","SicEff",
            "icEff","beta0","dpHIC","dpLIC"),
  data=dataList,
#  inits=initsList,
  n.chains=nChainsDefault,
  adapt=1000,
  burnin=1500,
  sample=2000,
  thin=5,
  summarise=FALSE, #Diagnostics for everything? British spelling!
  plots=FALSE
)

#create list of samples from
#  all the separate chains
codaSamples = as.mcmc.list(runJagsOut)

#Combine chains into one big matrix 
allSamples_dp=do.call(rbind,codaSamples)

chainStats=summary(runJagsOut)

# Examine the chains:
# Convergence diagnostics:
seeDiag=function(parName){
  diagMCMC( codaObject=codaSamples , parName=parName )  
}


# Posterior plots
seePost=function(parName){
  hist(allSamples[,parName],40,
       main="",xlab=parName)
}

seeFancyPost=function(parName){
  dev.new(height=5,width=5,
          units="in",noRStudioGD = T) #create new plot window
  plotPost( codaSamples[,parName] , 
            main=parName , 
            xlab=bquote(mu[hic]) , 
            cenTend="median" , 
            #compVal=0.5 , 
            #ROPE=c(0.45,0.55) , 
            credMass=0.90 
  )
}

postCheckTheta=function(){
  par(mfcol=c(2,1),mar=c(5,5,1,1))
  
  lev=grep("thetaHIC",colnames(allSamples))
  obsHic=c()
  loHic=c()
  hiHic=c()
  for(i in 1:length(lev)){
    int=HDIofMCMC(allSamples[,lev[i]])
    loHic[i]=int[1]
    hiHic[i]=int[2]
    obsHic[i]=hh[i]/htot[i]
  }
  
  lev=grep("thetaLIC",colnames(allSamples))
  obsLic=c()
  loLic=c()
  hiLic=c()
  for(i in 1:length(lev)){
    int=HDIofMCMC(allSamples[,lev[i]])
    loLic[i]=int[1]
    hiLic[i]=int[2]
    obsLic[i]=lh[i]/ltot[i]
  }
  
  ord=order(obsHic)
  plot(obsHic[ord],pch=4)
  for(i in 1:length(ord)){
    lines(c(i,i),c(loHic[ord[i]],hiHic[ord[i]]),
          col="light blue")
  }
  #points(obsHic[ord],pch=4)
  
  ord=order(obsLic)
  plot(obsLic[ord],pch=4)
  for(i in 1:length(ord)){
    lines(c(i,i),c(loLic[ord[i]],hiLic[ord[i]]),
          col="light blue")
  }
  #points(obsLic[ord],pch=4)
  
  par(mfcol=c(1,1))
}


postCheckDist=function(){
  Sbeta0 = allSamples[,'Sbeta0']
  SicEff = allSamples[,'SicEff']
  lev=grep("dsMbeta0",colnames(allSamples))
  dsMbeta0=allSamples[,lev]
  lev=grep("dsMicEff",colnames(allSamples))
  dsMicEff=allSamples[,lev]
  
  obsHic=c()
  obsLic=c()
  loHic=c()
  loLic=c()
  hiHic=c()
  hiLic=c()
  
  for(i in 1:max(dataset)){
    obsHic[i]=sum(hh[dataset==i])/
      sum(htot[dataset==i])
    obsLic[i]=sum(lh[dataset==i])/
      sum(ltot[dataset==i])
    
    N=sum(dataset==i)
    beta0=matrix(NA,nrow(dsMbeta0),N)
    icEff=matrix(NA,nrow(dsMbeta0),N)
    
    for(j in 1:nrow(beta0)){
      beta0[j,]=rnorm(N,
                  dsMbeta0[j,i],Sbeta0)
      icEff[j,]=rnorm(N,
                      dsMicEff[j,i],SicEff)
    }
    
    logitHic=beta0+.5*icEff
    logitLic=beta0-.5*icEff
    
    thetaHic=1/(1+exp(-logitHic))
    thetaLic=1/(1+exp(-logitLic))
    
    hcsHic=matrix(NA,nrow(dsMbeta0),N)
    hcsLic=matrix(NA,nrow(dsMbeta0),N)
    pHic=c()
    pLic=c()
    for(j in 1:nrow(beta0)){
      hcsHic[j,]=rbinom(N,htot[dataset==i],thetaHic[j,])
      hcsLic[j,]=rbinom(N,ltot[dataset==i],thetaLic[j,])
      pHic[j]=sum(hcsHic[j,])/sum(htot[dataset==i])
      pLic[j]=sum(hcsLic[j,])/sum(ltot[dataset==i])
    }
    
    int=HDIofMCMC(pHic)
    loHic[i]=int[1]
    hiHic[i]=int[2]
    
    int=HDIofMCMC(pLic)
    loLic[i]=int[1]
    hiLic[i]=int[2]
  }
}

#d prime distributions for high and low confidence 
dpHIC = allSamples_dp[,'Mbeta0'] + .5*allSamples_dp[,'MicEff']
hist(dpHIC, c = "darksalmon", xlab = "d prime for HIC")
med_all_samps = median(dpHIC)
abline(v=med_all_samps,col="black",lwd=2)

dpLIC = allSamples_dp[,'Mbeta0'] - .5*allSamples_dp[,'MicEff']
hist(dpLIC, c = "darksalmon", xlab = "d prime for LIC")
med_all_samps = median(dpLIC)
abline(v=med_all_samps,col="black",lwd=2)

# Take median d'prime for each participant for low and high item confidence
# correlate with betas
part_err_betas = allSamples_err[ ,grep("^beta0", colnames(allSamples_err))]
part_err_betas_meds = apply(part_err_betas, 2, median)

part_err_ice = allSamples_err[ ,grep("^icEff", colnames(allSamples_err))]
part_err_ice_meds = apply(part_err_ice, 2, median)

part_corr_betas = allSamples_corr[ ,grep("^beta0", colnames(allSamples_corr))]
part_corr_betas_meds = apply(part_corr_betas, 2, median)

part_corr_ice = allSamples_corr[ ,grep("^icEff", colnames(allSamples_corr))]
part_corr_ice_meds = apply(part_corr_ice, 2, median)

part_dpHIC = allSamples_dp[ ,grep("dpHIC", colnames(allSamples_dp))]
part_dpHIC_meds = apply(part_dpHIC, 2, median)

part_dpLIC = allSamples_dp[ ,grep("dpLIC", colnames(allSamples_dp))]
part_dpLIC_meds = apply(part_dpLIC, 2, median)

#plots!!!
cor(part_corr_betas_meds, part_dpHIC_meds)
plot(part_corr_betas_meds, part_dpHIC_meds, main = "d'prime for HIGH item confidence and Error beta estimates",
     xlab="Median Beta Estimates \n (per participant)", ylab="Median dprime for High item confidence")
text(x = 0.0, y = 3, label = "Pearson R: 0.9606")

cor(part_corr_betas_meds, part_dpLIC_meds)
plot(part_corr_betas_meds, part_dpLIC_meds, main = "d'prime for LOW item confidence and beta estimates",
     xlab="Median Beta Estimates \n (per participant)", ylab="Median dprime for Low item confidence")
text(x = 0.0, y = 1.6, label = "Pearson R: 0.9084")

# correlate ICE to low and high dp
cor(part_corr_ice_meds, part_dpHIC_meds)
plot(part_corr_ice_meds, part_dpHIC_meds, main = "d'prime for HIGH item confidence and ICE",
     xlab="Median ICE (per participant)", ylab="Median dprime for High item confidence")
text(x = 0.1, y = 3, label = "Pearson R: 0.7551")

cor(part_corr_ice_meds, part_dpLIC_meds)
plot(part_err_ice_meds, part_dpLIC_meds, main = "d'prime for LOW item confidence and ICE",
     xlab="Median ICE (per participant)", ylab="Median dprime for Low item confidence")
text(x = 0.1, y = 1.6, label = "Pearson R: 0.1431")

# change in dprime from low to high item confidence 
delta_dp <- part_dpHIC_meds - part_dpLIC_meds
cor(delta_dp, part_corr_ice_meds)
plot(delta_dp, part_corr_ice_meds, main = "delta d'prime and ICE",
     xlab="Median ICE", ylab="delta dprime")
text(x = 0.1, y = 1.6, label = "Pearson R: 0.999")



setwd("~/Documents/dataFiles_masters/")
source("Source Error Confidence/DBDA2Eprograms/DBDA2E-utilities.R")

#setwd("~/Johanny/")
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceIncorrect_asymmetric")

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

library("dplyr")
data_all <- list.files(path = getwd(), pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 
dat=as.matrix(data_all)

setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceCorrect_asymmetric")
data_all_c <- list.files(path = getwd(), pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 
datc=as.matrix(data_all_c)

#source 1 incorrect
hh_s1=as.numeric(dat[,'HChit_HCE_S1'])
hl_s1=as.numeric(dat[,'HChit_LCE_S1'])
lh_s1=as.numeric(dat[,'LChit_HCE_S1'])
ll_s1=as.numeric(dat[,'LChit_LCE_S1'])

#source 2 incorrect
hh_s2=as.numeric(dat[,'HChit_HCE_S2'])
hl_s2=as.numeric(dat[,'HChit_LCE_S2'])
lh_s2=as.numeric(dat[,'LChit_HCE_S2'])
ll_s2=as.numeric(dat[,'LChit_LCE_S2'])

#source 1 correct
hhc_s1=as.numeric(datc[,'HChit_HCC_S1'])
hlc_s1=as.numeric(datc[,'HChit_LCC_S1'])
lhc_s1=as.numeric(datc[,'LChit_HCC_S1'])
llc_s1=as.numeric(datc[,'LChit_LCC_S1'])

#source 2 correct
hhc_s2=as.numeric(datc[,'HChit_HCC_S2'])
hlc_s2=as.numeric(datc[,'HChit_LCC_S2'])
lhc_s2=as.numeric(datc[,'LChit_HCC_S2'])
llc_s2=as.numeric(datc[,'LChit_LCC_S2'])

#source1 correct and incorrect
s1c_tot = hhc_s1 + hlc_s1 + lhc_s1 + llc_s1
s1_tot = hh_s1 + hl_s1 + lh_s1 + ll_s1

#source2 correct and incorrect
s2c_tot = hhc_s2 + hlc_s2 + lhc_s2 + llc_s2
s2_tot = hh_s2 + hl_s2 + lh_s2 + ll_s2

#source1 total
s1_totb = s1_tot + s1c_tot

#source2 total
s2_totb = s2_tot + s2c_tot

s1_HCI = hhc_s1 + hlc_s1
s1_LCI = lhc_s1 + llc_s1

s2_HCI = hhc_s2 + hlc_s2
s2_LCI = lhc_s2 + llc_s2

sCor_s1 = s1_HCI + s1_LCI
sCor_s2 = s2_HCI + s2_LCI

dataset=dat[,'Study']
datasetc=datc[,'Study']
#these should match. To check:
# cbind(dataset,datasetc)


dataList = list(    # Put the information into a list.
  sCor_s1 = s1c_tot,
  sCor_s2 = s2c_tot,
  s2_totb = s2_totb,
  s1_totb = s1_totb,
  # hh = hh,
  # lh = lh,
  # hhc = hhc,
  # lhc = lhc,
  # htot = htot,
  # ltot = ltot,
  # htotc = htotc,
  # ltotc = ltotc,
  # htotb = htotb,
  # ltotb = ltotb,
  dataset = dat[,'Study'],
  nDataset = max(dat[,'Study']),
  part = 1:nrow(dat) ,
  nPart = nrow(dat) 
)


# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    sCor_s1[i] ~ dbinom( thetaHIC[i], s1_totb[i] )
    sCor_s2[i] ~ dbinom( thetaLIC[i], s2_totb[i] )

    thetaHIC[i] = 1-pnorm(0, dpS1[i]/2, 1/ 1 ^2)
    thetaLIC[i] = 1-pnorm(0, dpS2[i]/2, 1/ 1 ^2)
    
    dpS1[i] = beta0[i] + .5*icEff[i] 
    dpS2[i] = beta0[i] - .5*icEff[i] 
    
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
            "icEff","beta0","dpS1","dpS2"),
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
allSamples_asym=do.call(rbind,codaSamples)

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
dps1 = allSamples_asym[,'Mbeta0'] + .5*allSamples_asym[,'MicEff']
hist(dps1, c = "darksalmon", xlab = "d prime for HIC")
med_all_samps = median(dps1)
abline(v=med_all_samps,col="black",lwd=2)

dps2 = allSamples_asym[,'Mbeta0'] - .5*allSamples_asym[,'MicEff']
hist(dps2, c = "darksalmon", xlab = "d prime for LIC")
med_all_samps = median(dps2)
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

part_dps1 = allSamples_asym[ ,grep("dpS1", colnames(allSamples_asym))]
part_dps1_meds = apply(part_dps1, 2, median)

part_dps2 = allSamples_asym[ ,grep("dpS2", colnames(allSamples_asym))]
part_dps2_meds = apply(part_dps2, 2, median)





# load the set of useful
#  functions that Kruschke coded
setwd("~/Documents/dataFiles_masters/")
source("Source Error Confidence/DBDA2Eprograms/DBDA2E-utilities.R")

#setwd("~/Johanny/")
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/SourceIncorrect_balanced/collapsedStrength")

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
# dat1=read.csv("SourceErrorConfidence/Data_reformatted_sourceIncorr/collapsedStrength/SandersExp1_collapseStrength.csv",
#               stringsAsFactors = F)
# dat1=as.matrix(dat1)
# dat=rbind(dat1,dat2,dat3)

# get number source corrects 
# get source d prime for high and low item confidence 
# repeat model for source correct 

# loads all the data in a given folder and concatenates it in one matrix
library("dplyr")
data_all <- list.files(path = getwd(), pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 
dat=as.matrix(data_all)

hh=as.numeric(dat[,'HC.hit.HC.error'])
hl=as.numeric(dat[,'HC.hit.LC.error'])
lh=as.numeric(dat[,'LC.hit.HC.error'])
ll=as.numeric(dat[,'LC.hit.LC.error'])

htot=hh+hl
ltot=lh+ll

dataList = list(    # Put the information into a list.
  hh = hh,
  lh = lh,
  htot = htot,
  ltot = ltot,
  dataset = dat[,'Study'],
  nDataset = max(dat[,'Study']),
  part = 1:nrow(dat) ,
  nPart = nrow(dat) 
)


# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    hh[i] ~ dbinom( thetaHIC[i], htot[i] )
    lh[i] ~ dbinom( thetaLIC[i], ltot[i] )
    
    thetaHIC[i] = exp(logitHIC[i])/(1+exp(logitHIC[i]))
    thetaLIC[i] = exp(logitLIC[i])/(1+exp(logitLIC[i]))
    
    logitHIC[i] = beta0[i] + .5*icEff[i] 
    logitLIC[i] = beta0[i] - .5*icEff[i] 
    
    beta0[i] ~ dnorm(dsMbeta0[dataset[i]] , 1/ Sbeta0  ^2)
    icEff[i] ~ dnorm(dsMicEff[dataset[i]], 1/ SicEff  ^2)
  }
  
  for(j in 1:nDataset){
    dsMbeta0[j] ~ dnorm(Mbeta0, 1/ SdsBeta0 ^2)
    dsMicEff[j] ~ dnorm(MicEff, 1/ SdsIcEff ^2)
  }
  
  Mbeta0 ~ dnorm( 0 , 1/ 1.5 ^2)
  Sbeta0 ~ dnorm( .4 , 1/ .4 ^2)T(.1,2)
  MicEff ~ dnorm( 0 , 1/ .5 ^2)
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
  monitor=c("Mbeta0","MicEff","beta0","icEff","Sbeta0","SicEff",
            "dsMbeta0","dsMicEff","SdsBeta0",
            "SdsIcEff"),
  data=dataList,
# inits=initsList,
  n.chains=nChainsDefault,
  adapt=1000,
  burnin=1500,
  sample=4000,
  thin=5,
  summarise=FALSE, #Diagnostics for everything? British spelling!
  plots=FALSE
)

#create list of samples from
#  all the separate chains
codaSamples = as.mcmc.list(runJagsOut)

#Combine chains into one big matrix 
allSamples_err=do.call(rbind,codaSamples)

chainStats=summary(runJagsOut)

# Examine the chains:
# Convergence diagnostics:
# There's an error here; need to install XQuartz?
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

# overall ICE
hist(allSamples_corr[,"MicEff"], c = "darksalmon", main = "Item Confidence Effect \n for CORRECT source judgements", xlab = "Mean item confidence effect (log odds)", xlim = c(1, 3))
med_all_samps = median(allSamples_corr[,"MicEff"])
abline(v=med_all_samps,col="black",lwd=2)
text(x = 0.8, y = 2500, label = "Reference probability: 0.797")
#mtext("Reference probability: 0.797", side = 3)

# by dataset; need to change titles
# change x-axis here 
par(mfrow=c(3,4))
hist(allSamples_err[,"dsMicEff[1]"], main = "Sanders Exp.1", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[2]"], main = "Sanders Exp.2", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[3]"], main = "SK Exp.3", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[4]"], main = "Sk Exp 4.1", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[5]"], main = "Sk Exp 4.2", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[6]"], main = "Pazzaglia Exp. 2", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[7]"], main = "Pazzaglia Exp. 3", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[8]"], main = "Pazzaglia Exp. 4", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[9]"], main = "Fox Exp. 1a Blocked", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[10]"], main = "Fox Exp. 1a Reversed", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[11]"], main = "Fox Exp. 1a Sim",c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))
hist(allSamples_err[,"dsMicEff[12]"], main = "Fox Exp. 3a Sim", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 3))

par(mfrow=c(1,1))
hist(allSamples_err[,"dsMicEff[10]"], main = "Fox Exp. 1a Reversed", c = "darksalmon", xlab = "Mean ICE (log odds)", xlim = c(0, 2.5))
med_all_samps = median(allSamples_err[,"dsMicEff[10]"])
abline(v=med_all_samps,col="black",lwd=2)
text(x = 0.5, y = 1500, label = "Reference probability: \n 0.775")

# overall intercept - overall log odds of reporting high confidence for a source error
hist(allSamples_err[,"Mbeta0"], c = "darksalmon", main = "Mean Intercept \n for INCORRECT source judgements", xlab = "Mean intercept (log odds)")
med_all_samps = median(allSamples_err[,"Mbeta0"])
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -1.9, y = 3000, label = "Reference probability: \n 0.183")

# intercept by dataset
par(mfrow=c(3,4))
hist(allSamples[,"dsMbeta0[1]"], main = "Sanders Exp.1", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[2]"], main = "Sanders Exp.2", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[3]"], main = "SK Exp.3", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[4]"], main = "Sk Exp 4.1", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[5]"], main = "Sk Exp 4.2", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[6]"], main = "Pazzaglia Exp. 2", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[7]"], main = "Pazzaglia Exp. 3", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[8]"], main = "Pazzaglia Exp. 4", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[9]"], main = "Fox Exp. 1a Blocked", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[10]"], main = "Fox Exp. 1a Reversed", c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[11]"], main = "Fox Exp. 1a Sim",c = "darksalmon", xlab = "Mean intercept (log odds)")
hist(allSamples[,"dsMbeta0[12]"], main = "Fox Exp. 3a Sim", c = "darksalmon", xlab = "Mean intercept (log odds)")

#high/low item conf intercepts
logOddsHIC = allSamples_corr[,'Mbeta0'] + .5*allSamples_corr[,'MicEff']
hist(logOddsHIC, main = "Mean Intercept for HIGH item confidence \n (source correct)", c = "darksalmon", xlab = "Mean intercept (log odds)")
med_all_samps = median(logOddsHIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -0.1, y = 2100, label = "Median probability: \n 0.631")

logOddsLIC = allSamples_corr[,'Mbeta0'] -.5*allSamples_corr[,'MicEff']
hist(logOddsLIC, main = "Mean Intercept for LOW item confidence \n (source correct)", c = "darksalmon", xlab = "Mean intercept (log odds)")
med_all_samps = median(logOddsLIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -2.0, y = 2100, label = "Median probability: \n 0.189")




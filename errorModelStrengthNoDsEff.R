
# load the set of useful
#  functions that Kruschke coded
setwd("~/Documents/dataFiles_masters/")
source("Source Error Confidence/DBDA2Eprograms/DBDA2E-utilities.R")

detectCores() #see how many cores you have
# Kruschke's utilities code sets best
#  default chain #'s and methods for
#  your machine. Variables that hold
#  those values:
nChainsDefault
runjagsMethodDefault

#wd
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceIncorrect_balanced/strengthOrder")

#logistic functions
logistic=function(logit) exp(logit)/(1+exp(logit))
logit=function(p) log(p/(1-p))

# Load the data:
datw=read.csv("Pazzaglia_E2_W.csv")
dats=read.csv("Pazzaglia_E2_S.csv")

new=read.csv("Pazzaglia_E3_W.csv")
datw=rbind(datw,new)
new=read.csv("Pazzaglia_E3_S.csv")
dats=rbind(dats,new)

new=read.csv("Pazzaglia_E4_W.csv")
datw=rbind(datw,new)
new=read.csv("Pazzaglia_E4_S.csv")
dats=rbind(dats,new)

new=read.csv("SK_ex4_t1_NR.csv")
datw=rbind(datw,new)
new=read.csv("SK_ex4_t1_SS.csv")
dats=rbind(dats,new)

# new=read.csv("SandersExp1_Strength_0.csv")
# datw=rbind(datw,new)
# new=read.csv("SandersExp1_Strength_1.csv")
# dats=rbind(dats,new)
# 
# new=read.csv("SandersExp2_Strength_0.csv")
# datw=rbind(datw,new)
# new=read.csv("SandersExp2_Strength_1.csv")
# dats=rbind(dats,new)

dat=rbind(datw,dats)

hh=as.numeric(datw[,'HC.hit.HC.error'])
hl=as.numeric(datw[,'HC.hit.LC.error'])
lh=as.numeric(datw[,'LC.hit.HC.error'])
ll=as.numeric(datw[,'LC.hit.LC.error'])

hhs=as.numeric(dats[,'HC.hit.HC.error'])
hls=as.numeric(dats[,'HC.hit.LC.error'])
lhs=as.numeric(dats[,'LC.hit.HC.error'])
lls=as.numeric(dats[,'LC.hit.LC.error'])

############### repeat for source correct
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceCorrect_balanced/strengthOrder")

datw=read.csv("Pazzaglia_E2_W_sourceCorr.csv")
dats=read.csv("Pazzaglia_E2_S_sourceCorr.csv")

new=read.csv("Pazzaglia_E3_W_sourceCorr.csv")
datw=rbind(datw,new)
new=read.csv("Pazzaglia_E3_S_sourceCorr.csv")
dats=rbind(dats,new)

new=read.csv("Pazzaglia_E4_W_sourceCorr.csv")
datw=rbind(datw,new)
new=read.csv("Pazzaglia_E4_S_sourceCorr.csv")
dats=rbind(dats,new)

new=read.csv("SK_ex4_t1_NR_sourceCorr.csv")
datw=rbind(datw,new)
new=read.csv("SK_ex4_t1_SS_sourceCorr.csv")
dats=rbind(dats,new)

# new=read.csv("SandersExp1_Strength_0_sourceCorrect.csv")
# datw=rbind(datw,new)
# new=read.csv("SandersExp1_Strength_1_sourceCorrect.csv")
# dats=rbind(dats,new)
# 
# new=read.csv("SandersExp2_Strength_0_sourceCorrect.csv")
# datw=rbind(datw,new)
# new=read.csv("SandersExp2_Strength_1_sourceCorrect.csv")
# dats=rbind(dats,new)

dat=rbind(datw,dats)

hh=as.numeric(datw[,'HC.hit.HC.corr'])
hl=as.numeric(datw[,'HC.hit.LC.corr'])
lh=as.numeric(datw[,'LC.hit.HC.corr'])
ll=as.numeric(datw[,'LC.hit.LC.corr'])

hhs=as.numeric(dats[,'HC.hit.HC.corr'])
hls=as.numeric(dats[,'HC.hit.LC.corr'])
lhs=as.numeric(dats[,'LC.hit.HC.corr'])
lls=as.numeric(dats[,'LC.hit.LC.corr'])

##############
htot=hh+hl
ltot=lh+ll

htots=hhs+hls
ltots=lhs+lls

pHcWkHic = hh/htot
pHcWkLic = lh/ltot
pHcStHic = hhs/htots
pHcStLic = lhs/ltots


dataList = list(    # Put the information into a list.
  wh = hh,
  sh = hhs,
  wtot = htot,
  stot = htots,
  part = 1:nrow(datw) ,
  nPart = nrow(datw) 
)


# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    sh[i] ~ dbinom( thetaSt[i], stot[i] )
    wh[i] ~ dbinom( thetaWk[i], wtot[i] )
    
    thetaSt[i] = exp(logitSt[i])/(1+exp(logitSt[i]))
    thetaWk[i] = exp(logitWk[i])/(1+exp(logitWk[i]))
    
    logitSt[i] = beta0[i] + .5*strEff[i] 
    logitWk[i] = beta0[i] - .5*strEff[i] 
    
    beta0[i] ~ dnorm(Mbeta0 , 1/ Sbeta0  ^2)
    strEff[i] ~ dnorm(MstrEff, 1/ SstrEff  ^2)
  }
  
  Mbeta0 ~ dnorm( 0 , 1/ 1.5 ^2)
  Sbeta0 ~ dnorm( .4 , 1/ .4 ^2)T(.1,2)
  MstrEff ~ dnorm( 0 , 1/ 1.5 ^2)
  SstrEff ~ dnorm( .4 , 1/ .2 ^2)T(.1,1)

}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )


# Run the chains:
runJagsOut = run.jags(
  method=runjagsMethodDefault, #"parallel" or "rjags" 
  model="TEMPmodel.txt",
  monitor=c("Mbeta0","MstrEff","Sbeta0","SstrEff",
            "beta0","strEff",
            "thetaSt","thetaWk"),
  data=dataList,
#  inits=initsList,
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
allSamples_str=do.call(rbind,codaSamples)

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
  par(mfcol=c(2,1),mar=c(4,5,1,1))
  
  lev=grep("thetaSt",colnames(allSamples))
  obsSt=c()
  loSt=c()
  hiSt=c()
  for(i in 1:length(lev)){
    int=HDIofMCMC(allSamples[,lev[i]])
    loSt[i]=int[1]
    hiSt[i]=int[2]
    obsSt[i]=dataList$sh[i]/dataList$stot[i]
  }
  
  lev=grep("thetaWk",colnames(allSamples))
  obsWk=c()
  loWk=c()
  hiWk=c()
  for(i in 1:length(lev)){
    int=HDIofMCMC(allSamples[,lev[i]])
    loWk[i]=int[1]
    hiWk[i]=int[2]
    obsWk[i]=dataList$wh[i]/dataList$wtot[i]
  }
  
  ord=order(obsSt)
  plot(obsSt[ord],pch=4)
  for(i in 1:length(ord)){
    lines(c(i,i),c(loSt[ord[i]],hiSt[ord[i]]),
          col="light blue")
  }
  #points(obsHic[ord],pch=4)
  
  ord=order(obsWk)
  plot(obsWk[ord],pch=4)
  for(i in 1:length(ord)){
    lines(c(i,i),c(loWk[ord[i]],hiWk[ord[i]]),
          col="light blue")
  }
  #points(obsLic[ord],pch=4)
  
  par(mfcol=c(1,1))
}


# overall ICE for source incorrect
hist(allSamples_str[,"MstrEff"], c = "darksalmon", main = "Item Strength Effect \n for High Item confidence", xlab = "Mean item strength effect (log odds)", breaks = 18)
med_all_samps = median(allSamples_str[,"MstrEff"])
abline(v=med_all_samps,col="black",lwd=2)
text(x = 0.05, y = 2200, label = "Median strength Effect: \n 0.290")

# overall intercept for source incorr
hist(allSamples_str[,"Mbeta0"], c = "darksalmon", main = "Mean Intercept \n for High Item confidence", xlab = "Mean intercept (log odds)")
med_all_samps = median(allSamples_str[,"Mbeta0"])
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -0.73, y = 1500, label = "Median probability: \n 0.400")

#high/low item conf intercepts source incorr 
par(mfrow=c(1,2))
logOddsLIC = allSamples_str[,'Mbeta0'] -.5*allSamples_str[,'MstrEff']
hist(logOddsLIC, main = "Mean Intercept for Weak Items \n (source incorrect)", c = "darksalmon", xlab = "Mean intercept (log odds)", breaks = 20)
med_all_samps = median(logOddsLIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -0.90, y = 1500, label = "Median probability: \n 0.366")

logOddsHIC = allSamples_str[,'Mbeta0'] + .5*allSamples_str[,'MstrEff']
hist(logOddsHIC, main = "Mean Intercept for Strong Items \n (source incorrect)", c = "darksalmon", xlab = "Mean intercept (log odds)")
med_all_samps = median(logOddsHIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = -0.60, y = 1500, label = "Median probability: \n 0.436")

############
# overall ICE for source correct
hist(allSamples_str[,"MstrEff"], c = "darksalmon", main = "Item Strength Effect \n for CORRECT source judgements", xlab = "Mean item strength effect (log odds)")
med_all_samps = median(allSamples_str[,"MstrEff"])
abline(v=med_all_samps,col="black",lwd=2)
text(x = 0.45, y = 2500, label = "Median strength Effect: \n 0.663")

# overall intercept for source correct
hist(allSamples_str[,"Mbeta0"], c = "darksalmon", main = "Mean Intercept \n for CORRECT source judgements", xlab = "Mean intercept (log odds)")
med_all_samps = median(allSamples_str[,"Mbeta0"])
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = 0.95, y = 1500, label = "Median probability: \n 0.774")

#high/low item conf intercepts source correct 
par(mfrow=c(1,2))
logOddsLIC = allSamples_str[,'Mbeta0'] -.5*allSamples_str[,'MstrEff']
hist(logOddsLIC, main = "Mean Intercept for Weak Items \n (source correct)", c = "darksalmon", xlab = "Mean intercept (log odds)")
med_all_samps = median(logOddsLIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = 0.60, y = 1700, label = "Median probability: \n 0.711")

logOddsHIC = allSamples_str[,'Mbeta0'] + .5*allSamples_str[,'MstrEff']
hist(logOddsHIC, main = "Mean Intercept for Strong Items \n (source correct)", c = "darksalmon", xlab = "Mean intercept (log odds)")
med_all_samps = median(logOddsHIC)
abline(v=med_all_samps,col="black",lwd=2)
logistic(med_all_samps)
text(x = 1.30, y = 1700, label = "Median probability: \n 0.827")





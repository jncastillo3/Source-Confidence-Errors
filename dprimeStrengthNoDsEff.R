setwd("~/Johanny/")

# load the set of useful
#  functions that Kruschke coded
setwd("~/Documents/dataFiles_masters/")
source("Source Error Confidence/DBDA2Eprograms/DBDA2E-utilities.R")
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceCorrect_balanced/strengthOrder")

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

####################
setwd("~/Documents/dataFiles_masters/SourceErrorConfidence/sourceIncorrect_balanced/strengthOrder")
datwe=read.csv("Pazzaglia_E2_W.csv")
datse=read.csv("Pazzaglia_E2_S.csv")

newe=read.csv("Pazzaglia_E3_W.csv")
datwe=rbind(datwe,newe)
newe=read.csv("Pazzaglia_E3_S.csv")
datse=rbind(datse,newe)

newe=read.csv("Pazzaglia_E4_W.csv")
datwe=rbind(datwe,newe)
newe=read.csv("Pazzaglia_E4_S.csv")
datse=rbind(datse,newe)

newe=read.csv("SK_ex4_t1_NR.csv")
datwe=rbind(datwe,newe)
newe=read.csv("SK_ex4_t1_SS.csv")
datse=rbind(datse,newe)

# newe=read.csv("SandersExp1_Strength_0.csv")
# datwe=rbind(datwe,newe)
# newe=read.csv("SandersExp1_Strength_1.csv")
# datse=rbind(datse,newe)
# 
# newe=read.csv("SandersExp2_Strength_0.csv")
# datwe=rbind(datwe,newe)
# newe=read.csv("SandersExp2_Strength_1.csv")
# datse=rbind(datse,newe)

dat=rbind(datwe,datse)

hhwe=as.numeric(datwe[,'HC.hit.HC.error'])
hlwe=as.numeric(datwe[,'HC.hit.LC.error'])
lhwe=as.numeric(datwe[,'LC.hit.HC.error'])
llwe=as.numeric(datwe[,'LC.hit.LC.error'])

hhse=as.numeric(datse[,'HC.hit.HC.error'])
hlse=as.numeric(datse[,'HC.hit.LC.error'])
lhse=as.numeric(datse[,'LC.hit.HC.error'])
llse=as.numeric(datse[,'LC.hit.LC.error'])

hhw=as.numeric(datw[,'HC.hit.HC.corr'])
hlw=as.numeric(datw[,'HC.hit.LC.corr'])
lhw=as.numeric(datw[,'LC.hit.HC.corr'])
llw=as.numeric(datw[,'LC.hit.LC.corr'])

hhs=as.numeric(dats[,'HC.hit.HC.corr'])
hls=as.numeric(dats[,'HC.hit.LC.corr'])
lhs=as.numeric(dats[,'LC.hit.HC.corr'])
lls=as.numeric(dats[,'LC.hit.LC.corr'])

# Stc_tot = hhs + hls + lhs + lls
# Stin_tot = hhse + hlse + lhse + llse
# 
# Wkc_tot = hhw + hlw + lhw + llw
# Wkin_tot = hhwe + hlwe + lhwe + llwe
# 
# St_totb = Stc_tot + Stin_tot
# Wk_totb = Wkc_tot + Wkin_tot

Stc_tot = hhs
Stin_tot = hhse

Wkc_tot = hhw
Wkin_tot = hhwe

St_totb = Stc_tot + Stin_tot
Wk_totb = Wkc_tot + Wkin_tot

dataList = list(    # Put the information into a list.
  Stc_tot = Stc_tot,
  Wkc_tot = Wkc_tot,
  St_totb = St_totb,
  Wk_totb = Wk_totb,
  part = 1:nrow(datw) ,
  nPart = nrow(datw) 
)

# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    Stc_tot[i] ~ dbinom( thetaSt[i], St_totb[i] )
    Wkc_tot[i] ~ dbinom( thetaWk[i], Wk_totb[i] )
    
    thetaSt[i] = 1-pnorm(0, dpSt[i]/2, 1/ 1 ^2)
    thetaWk[i] = 1-pnorm(0, dpWk[i]/2, 1/ 1 ^2)
    
    dpSt[i] = beta0[i] + .5*strEff[i] 
    dpWk[i] = beta0[i] - .5*strEff[i] 
    
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
            "beta0","strEff", "dpSt", "dpWk"),
  data=dataList,
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
allSamples_dpstr=do.call(rbind,codaSamples)

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

hist(allSamples_str[,'MstrEff'])

#d prime distributions for strong and weak items 
par(mfrow=c(1,2))
par(mar = c(5.1, 4.1, 5, 2.1))
dpWk = allSamples_dpstr[,'Mbeta0'] - .5*allSamples_dpstr[,'MstrEff']
hist(dpWk, c = "darksalmon", xlab = "D' for weak items", breaks = 30, main = "Weak items")
med_all_samps = median(dpWk)
abline(v=med_all_samps,col="black",lwd=2)
text(x = 1.78, y = 1700, label = "Median probability: \n 2.083")
title("Item strength and D' conditioned on high confidence", line = -1, outer = TRUE)

dpStr = allSamples_dpstr[,'Mbeta0'] + .5*allSamples_dpstr[,'MstrEff']
hist(dpStr, c = "darksalmon", xlab = "D' for strong items", breaks = 30, main = "Strong items")
med_all_samps = median(dpStr)
abline(v=med_all_samps,col="black",lwd=2)
text(x = 2.36, y = 1700, label = "Median probability: \n 2.624")

#median d primes 
part_dpStr = allSamples_dpstr[ ,grep("dpSt", colnames(allSamples_dpstr))]
part_dpStr_meds = apply(part_dpStr, 2, median)

part_dpWk = allSamples_dpstr[ ,grep("dpWk", colnames(allSamples_dpstr))]
part_dpWk_meds = apply(part_dpWk, 2, median)

delta_dp <- part_dpStr_meds - part_dpWk_meds

part_err_ice = allSamples_str[ ,grep("^strEff", colnames(allSamples_str))]
part_err_ice_meds = apply(part_err_ice, 2, median)

cor(delta_dp, part_err_ice_meds)
par(mfrow=c(1,1))

plot(delta_dp, part_err_ice_meds, main = "delta d' and item Strength Effect",
     xlab="Median Strength Effects", ylab="delta d' (strong-weak item strength)", pch = 19, col = "darksalmon")
abline(lm(part_err_ice_meds~delta_dp), col = "black")
text(x = 0.7, y = 0.4, label = "Pearson R: -0.532")





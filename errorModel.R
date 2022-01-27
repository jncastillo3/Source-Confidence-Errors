

#setwd("~/Johanny/")
setwd("~/Downloads/Source Error Confidence/")

# load the set of useful
#  functions that Kruschke coded
source("DBDA2Eprograms/DBDA2E-utilities.R")

detectCores() #see how many cores you have
# Kruschke's utilities code sets best
#  default chain #'s and methods for
#  your machine. Variables that hold
#  those values:
nChainsDefault
runjagsMethodDefault


# Load the data:
dat=read.csv("FromJohanny/strength order/SandersExp1_Strength_1.csv")
dat=as.matrix(dat)

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
  part = 1:nrow(dat) ,
  nPart = nrow(dat) 
)


# Define the model:
modelString = "
model {
  for ( i in 1:nPart ) {
    hh[i] ~ dbinom( thetaHIC[i], htot[i] )
    lh[i] ~ dbinom( thetaLIC[i], ltot[i] )
    thetaHIC[i] ~ dnorm(Mhic, 1/ Shic  ^2)T(0,1)
    thetaLIC[i] ~ dnorm(Mlic, 1/ Slic  ^2)T(0,1)
  }
  Mhic ~ dnorm( .5 , 1/ 1 ^2)T(0,1)
  Shic ~ dnorm( .25 , 1/ .2 ^2)T(0.05,1)
  Mlic ~ dnorm( .5 , 1/ 1 ^2)T(0,1)
  Slic ~ dnorm( .25 , 1/ .2 ^2)T(0.05,1)
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )


# Run the chains:
runJagsOut = run.jags(
  method=runjagsMethodDefault, #"parallel" or "rjags" 
  model="TEMPmodel.txt",
  monitor=c("Mhic","Mlic","Shic","Slic","thetaHIC","thetaLIC"),
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
allSamples=do.call(rbind,codaSamples)

chainStats=summary(runJagsOut)

# Examine the chains:
# Convergence diagnostics:
diagMCMC( codaObject=codaSamples , parName="Mhic" )

# Posterior descriptives:
dev.new(height=5,width=5,
        units="in",noRStudioGD = T) #create new plpot window
plotPost( codaSamples[,"Mhic"] , 
          main="Mhic" , 
          xlab=bquote(mu[hic]) , 
          cenTend="median" , 
          #compVal=0.5 , 
          #ROPE=c(0.45,0.55) , 
          credMass=0.90 
          )

ice=allSamples[,'Mhic']-allSamples[,'Mlic']


#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHER YOU DO THINGS
dat.out <- "../data_processed/"
site.id <- "MortonArb"

Modis_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Site[loc[k]] + THRESH  #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], sPrec)
    }
        
    for(t in 1:nLoc){
    Site[t] <- Ex[t]
    Ex[t] ~ dnorm(0, aPrec)
    }

    aPrec ~ dgamma(0.1, 0.1)
    sPrec ~ dgamma(0.1, 0.1)
    THRESH ~ dnorm(200, .0001)
    
  }
  "

modis.list <- list(y = Greenup.dat$GDD5.cum, nObs = length(Greenup.dat$GDD5.cum),
                   loc = as.numeric(factor(Greenup.dat$site)), nLoc = length(unique(Greenup.dat$site)))


NPN_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Species[sp[k]] + THRESH #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(t in 1:nSp){
    Species[t] <- ind[pln[t]] + c[t]
    c[t] ~ dnorm(0, aPrec)
    }

    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    aPrec ~ dgamma(0.1, 0.1)
    bPrec ~ dgamma(0.1, 0.1)
    sPrec ~ dgamma(0.1, 0.1)
    THRESH ~ dnorm(200, .001)

  }
  "

#Individual_id is plant number equivalnet
#change this
NPN.list <- list(y = dat.comb$GDD5.cum, nObs = length(dat.comb$GDD5.cum) ,
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   sp = as.numeric(factor(dat.comb$Species)), nSp = length(unique(dat.comb$Species)))
                   


#Setting the number of MCMC chains and their parameters
nchain = 10
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(aPrec = runif(1,1/200,30), #Added length equal to number of species
                     sPrec = runif(1,1/200,30))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
burst.model   <- jags.model (file = textConnection(NPN_regression),
                             data = NPN.list,
                             inits = inits,
                             n.chains = 10)


#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("THRESH", "Species"," ind", "sPrec", "aPrec", "bPrec"),
                             n.iter = 100000)

DIC2 <- dic.samples(burst.model, 50000)

# #Checking that convergence happened
gelman.diag(burst.out)
# 
# #Checking where convergence occured
GBR <- gelman.plot(burst.out)

#Removing burnin before convergence occurred
burnin = 90000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in
plot(burst.burn)
summary(burst.burn)


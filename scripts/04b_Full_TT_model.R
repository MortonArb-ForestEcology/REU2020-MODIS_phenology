#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Bayesian model which will become the final product
# Inputs: Dataframe of MODIS or NPN data
# Outputs: Model Prediciton of the GDD5 threshold, and the effects of Species and Location on that Threshold
# Notes: This script is based on exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#
#This is the standard species hierarchical model designed for NPN data including a level for location
library(rjags)
library(coda)

# Read in output of previous script
dat.all <- read.csv("../data_processed/Phenology_NPN_combined.csv")
dat.all$Date <- as.Date(dat.all$Date)

#HERE IS WHERE YOU CAN MODIFY YOUR DATA FRAME FOR WHAT THE MODEL NEEDS


#These two lines aren't really neccessary but helps me keep track of what I'm working with
species <- c("Quercus imbricaria", "Quercus falcata", "Quercus stellata")
dat.comb <- dat.all[dat.all$Species %in% species, ]


hierarchical_regression <- "
  model{
    
    for(k in 1:nObs){
      mu[k] <- Base + Ex[loc[k]] + Species[sp[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(k in 1:nObs){
      Ynew[k]  ~ dnorm(munew[k], sPrec)
      munew[k] <- Base + Ex[loc[k]] + Species[sp[k]]
    }
    
    # Priors
    for(j in 1:nSp){                      #This loop adds the species effect on Threshold
    Species[j] ~ dnorm(0, tPrec)
    }
    
    for(t in 1:nLoc){
    Ex[t] <- ind[pln[t]] + c[t]
    c[t] ~ dnorm(0, aPrec)
    }
    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    tPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
    bPrec ~ dgamma(0.1, 0.1)
    sPrec ~ dgamma(0.1, 0.1)
    Base ~ dnorm(100, .01)
    
    d[1] <- max(Ynew[])
    d[2] <- min(Ynew[])
    d[3] <- max(Ynew[])-min(Ynew[])
    d[4] <- mean(Ynew[])
    d[5] <- sd(Ynew[])
  }
  "

#HERE IS WHERE YOU NEED TO CAREFULLY READ IN THE RIGHT COLUMNS FROM YOU DATA FRAME
#ONE KNOWN CHANGE IS THAT NPN DOESNT USE PLANTNUMBER BUT INDIVIDUAL_ID
burst.list <- list(y = dat.comb$GDD5.cum, sp = as.numeric(factor(dat.comb$Species)),
                   pln = as.numeric(factor(dat.comb$PlantNumber)), nPln = length(unique(dat.comb$PlantNumber)),
                   loc = as.numeric(factor(dat.comb$Site)), nLoc = length(unique(dat.comb$Site)),
                   nSp = length(unique(dat.comb$Species)), nObs = length(dat.comb$GDD5.cum))


#Setting the number of MCMC chains and their parameters
nchain = 10
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(b=rnorm(burst.list$nPln,0,5),
                     Species=rnorm(length(unique(dat.comb$Species)), 0, 5),  #Added length equal to number of species
                     sPrec = runif(1,1/200,30))
}

#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
burst.model   <- jags.model (file = textConnection(hierarchical_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 10)

#HERE YOU CAN DETERMINE WHAT VALUES YOU ARE PULLING OUT TO LOOK AT

#Converting the ooutput into a workable format
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("Species", "Base", "Ex"),
                             n.iter = 100000)


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



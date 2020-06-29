#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the basic Bayesian model which will be built off of
# Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script
# Outputs: Model Prediciton of the GDD5 threshold
# Notes: This script is based on exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/
#-----------------------------------------------------------------------------------------------------------------------------------#

#THIS IS WHER YOU DO THINGS
dat.out <- "../data_processed/"
site.id <- "MortonArb"

dat.comb <- read.csv(file.path(dat.out, paste0("MODIS_MET_", site.id, ".csv")))
dat.greenup <- dat.comb[dat.comb$BAND == 'Greenup',]
dat.midgreen <- dat.comb[dat.comb$BAND == 'MidGreenup',]
summary(dat.comb)

dat.npn <- read.csv("../data_processed/NPN/TEST_MortonArb_NPN_MET.csv")
summary(dat.npn)
head(dat.npn)


#---------------------------------------------------#
#This section sets up the model itself
#---------------------------------------------------#
#rjags for the model and coda for the summary statistics
library(rjags)
library(coda)
#YOU WILL NEED JAGS INSTALLED rjags is a package for interfacing but you need the program itself http://mcmc-jags.sourceforge.net/


#Setting up the Jags model itself
univariate_regression <- "
model{
  THRESH ~ dnorm(0, .001)
  Prec ~ dgamma(.1, .1)    ## prior precision 


  for(i in 1:n){
	  mu[i] <- THRESH                   ## process model ANOVA
	  y[i]  ~ dnorm(mu[i],Prec)		        ## data model
  }
}
"

#Checking how good of a predictor they currently seem
# plot(dat.comb$GDD5.cum, dat.comb$greenup.yday)

#------------------------------------------------------#
#This section converts our observed into the neccessary format, defines our uninformed prior, and sets up our MCMC chains
#------------------------------------------------------#
#Setting the number of MCMC chains and their parameters
nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(THRESH = rnorm(1,0,5), Prec = runif(1,1/200,1/20))
}


#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
#Converting to list format needed for JAGs
#THIS IS WHAT MATTERS TO ANDREW!!!!! == this is where we give it the data to do stats on
burst.list <- list(y = dat.comb$GDD5.cum, n = length(dat.comb$GDD5.cum))

#running the model
burst.model   <- jags.model (file = textConnection(univariate_regression),
                             data = burst.list,
                             inits = inits,
                             n.chains = 3)


#Converting the ooutput into a workable format
#DO THINGS HERE SOMETIMES
burst.out   <- coda.samples (model = burst.model,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)


#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
plot(burst.out)


#Checking that convergence happened
#1 is ideal especially where youll be
#below 1.05 is fine
gelman.diag(burst.out)


summary(burst.out)

mean(dat.comb$GDD5.cum)


#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
burst.burn <- window(burst.out,start=burnin)  ## remove burn-in


# save the part of the stats from when the model worked and converged
burst.df2 <- as.data.frame(as.matrix(burst.burn))
summary(burst.df2)
dim(burst.df2)

# re-creating the density plot ourselves
library(ggplot2)
ggplot(burst.df2) +
  geom_density(aes(x=THRESH))

# save the output
path.mod.out <- "../data_processed/mod.gdd5.MortonArb"
if(!dir.exists(path.mod.out)) dir.create(path.mod.out)
write.csv(burst.df2, file.path(path.mod.out, "THRESH_GDD5_MODIS_Greenup.csv"), row.names=F)

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

#Here is the different data to compare. All are constrained to GDD5.cum
#------------------------------
#MODIS Threshold Estimate, Greenup
dat.1 <- read.csv(file.path(dat.out, paste0("MODIS_MET_", site.id, ".csv")))
Greenup.dat <- dat.comb[dat.comb$BAND == 'Greenup',]
Greenup.gdd <- round(Greenup.dat$GDD5.cum)
head(Greenup.gdd)

#-----------------------------
#MODIS Threshold Estimate, MidGreenup

dat.2 <- read.csv(file.path(dat.out, paste0("MODIS_MET_", site.id, ".csv")))
MidGreenup.dat <- dat.comb[dat.comb$BAND == 'MidGreenup',]
MidGreenup.gdd <- round(MidGreenup.dat$GDD5.cum)
head(MidGreenup.gdd)

#-----------------------------
#NPN Threshold Estimate

NPN.dat <- read.csv(file.path(dat.out, paste0('TEST_', site.id, '_NPN_MET.csv')))
NPN.burst <- NPN.dat[NPN.dat$phenophase_id == '371',]
NPN.gdd <- NPN.burst$GDD5.cum
head(NPN.gdd)
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
#This is where we give it the data to do stats on

#Inputs = green.list , midgreen.list , NPN.list
#Inputs = Greenup.gdd , MidGreenup.gdd , NPN.gdd
green.list <- list(y = Greenup.gdd, n = length(Greenup.gdd))
midgreen.list <- list(y = MidGreenup.gdd, n = length(MidGreenup.gdd))
NPN.list <- list(y = NPN.gdd, n = length(NPN.gdd))

#running the model
#Inputs = green.mod , midgreen.mod , NPN.mod
green.mod   <- jags.model (file = textConnection(univariate_regression),
                             data = green.list,
                             inits = inits,
                             n.chains = 3)
midgreen.mod <- jags.model (file = textConnection(univariate_regression),
                            data = midgreen.list,
                            inits = inits,
                            n.chains = 3)
NPN.mod <- jags.model (file = textConnection(univariate_regression),
                       data = NPN.list,
                       inits = inits,
                       n.chains = 3)


#Converting the output into a workable format
#DO THINGS HERE SOMETIMES
green.out   <- coda.samples (model = green.mod,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
midgreen.out   <- coda.samples (model = midgreen.mod,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
NPN.out   <- coda.samples (model = NPN.mod,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)


#Trace plot and distribution. For trace make sure they are very overlapped showing convergence
#---------------------
#For Greenup.gdd
plot(green.out)
summary(green.out)

#For MidGreenup.gdd
plot(midgreen.out)
summary(midgreen.out)

#For NPN.gdd
plot(NPN.out)
summary(NPN.out)

#Checking that convergence happened
#1 is ideal, below 1.05 is fine
gelman.diag(green.out)
gelman.diag(midgreen.out)
gelman.diag(NPN.out)
#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
green.burn <- window(green.out, start= burnin)  ## remove burn-in
midgreen.burn <- window(midgreen.out, start= burnin)
NPN.burn <- window(NPN.out, start= burnin)


# save the part of the stats from when the model worked and converged
stats.greenup <- as.data.frame(as.matrix(green.burn))
summary(stats.greenup)
dim(stats.greenup)

stats.midgreenup <- as.data.frame(as.matrix(midgreen.burn))
summary(stats.midgreenup)
dim(stats.midgreenup)

stats.NPN <- as.data.frame(as.matrix(NPN.burn))
summary(stats.NPN)
dim(stats.NPN)

# re-creating the density plot
library(ggplot2)

ggplot(stats.greenup) +
  geom_density(aes(x=THRESH))

ggplot(stats.midgreenup) +
  geom_density(aes(x= THRESH))

ggplot(stats.NPN) +
  geom_density(aes(x= THRESH))

stats.all <- data.frame(greenup= stats.greenup,
                        midgreenup= stats.midgreenup, 
                        NPN= stats.NPN, 
                        stringsAsFactors=FALSE)
summary(stats.all)

#graph all the densities together to see overlap.
figures.dat <- '../figures'
if(!dir.exists(figures.dat)) dir.create(figures.dat)
png(width= 750, filename= file.path(figures.dat, 'THRESH_FIG_MortonArb.png'))
    
ggplot(data= stats.all) +
  ggtitle('Comparison of Growing Degree Day Thresholds at The Morton Arboretum') +
  geom_density(mapping = aes(x= greenup.THRESH, color = 'Greenup')) +
  geom_density(mapping = aes(x= midgreenup.THRESH, color = 'Midgreenup')) +
  geom_density(mapping = aes(x= NPN.THRESH, color = 'NPN')) +
  scale_x_continuous('THRESH (5C GDD)', breaks= seq(from= '-110', to='300', by= 20)) +
  scale_y_continuous('DENSITY (%)')
dev.off()

# save the outputs
path.mod.out <- "../data_processed/mod.gdd5.MortonArb"
if(!dir.exists(path.mod.out)) dir.create(path.mod.out)
write.csv(stats.greenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_Greenup.csv"), row.names=F)
write.csv(stats.midgreenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_MidGreenup.csv"), row.names=F)
write.csv(stats.NPN, file.path(path.mod.out, "THRESH_GDD5_NPN.csv"), row.names=F) 


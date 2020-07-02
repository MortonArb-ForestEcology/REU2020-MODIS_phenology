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
Greenup.dat <- dat.1[dat.1$BAND == 'Greenup',]
# Greenup.gdd <- round(Greenup.dat$GDD5.cum)
head(Greenup.gdd)

#-----------------------------
#MODIS Threshold Estimate, MidGreenup
dat.2 <- read.csv(file.path(dat.out, paste0("MODIS_MET_", site.id, ".csv")))
MidGreenup.dat <- dat.2[dat.2$BAND == 'MidGreenup',]
# MidGreenup.gdd <- round(MidGreenup.dat$GDD5.cum)
head(MidGreenup.gdd)

#-----------------------------
#NPN Threshold Estimate

alba.dat <- read.csv(file.path(dat.out, paste0('Alba_', site.id, '_NPN_MET.csv')))
alba.burst <- alba.dat[alba.dat$phenophase_id == '371',]
alba.gdd <- alba.burst$GDD5.cum
head(alba.gdd)
summary(alba.dat)

bur.dat <- read.csv(file.path(dat.out, paste0('Bur_', site.id, '_NPN_MET.csv')))
bur.burst <- bur.dat[bur.dat$phenophase_id == '371',]
bur.gdd <- bur.burst$GDD5.cum
head(bur.gdd)

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
  THRESH ~ dnorm(0, .0001)
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

#resolve
green.list <- list(y = dat.modis$GDD5.cum[dat.modis$BAND=="Greenup"], n = length(dat.modis$GDD5.cum[dat.modis$BAND=="Greenup"]))
midgreen.list <- list(y = dat.modis[dat.modis$BAND=="MidGreenup", "GDD5.cum"], n = length(dat.modis[dat.modis$BAND=="MidGreenup", "GDD5.cum"]))
#resolve
bur.list <- list(y = bur.gdd, n = length(bur.gdd))

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
alba.mod <- jags.model (file = textConnection(univariate_regression),
                       data = alba.list,
                       inits = inits,
                       n.chains = 3)
bur.mod <- jags.model (file = textConnection(univariate_regression),
                        data = bur.list,
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
alba.out   <- coda.samples (model = alba.mod,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
bur.out   <- coda.samples (model = bur.mod,
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
plot(alba.out)
summary(alba.out)

plot(bur.out)
summary(bur.out)

#Checking that convergence happened
#1 is ideal, below 1.05 is fine
gelman.diag(green.out)
gelman.diag(midgreen.out)
gelman.diag(alba.out)
gelman.diag(bur.out)
#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
green.burn <- window(green.out, start= burnin)  ## remove burn-in
midgreen.burn <- window(midgreen.out, start= burnin)
alba.burn <- window(alba.out, start= burnin)
bur.burn <- window(bur.out, start= burnin)


# save the part of the stats from when the model worked and converged
stats.greenup <- as.data.frame(as.matrix(green.burn))
summary(stats.greenup)
dim(stats.greenup)

stats.midgreenup <- as.data.frame(as.matrix(midgreen.burn))
summary(stats.midgreenup)
dim(stats.midgreenup)

stats.alba <- as.data.frame(as.matrix(alba.burn))
summary(stats.alba)
dim(stats.alba)

stats.bur <- as.data.frame(as.matrix(bur.burn))
summary(stats.bur)
dim(stats.bur)

# Once you have the information you NEED, you think about how to put it together

# re-creating the density plot
library(ggplot2)

ggplot(stats.greenup) +
  geom_density(aes(x=THRESH))

ggplot(stats.midgreenup) +
  geom_density(aes(x= THRESH))

ggplot(stats.alba) +
  geom_density(aes(x= THRESH))

ggplot(stats.bur) +
  geom_density(aes(x= THRESH))

# This is "wide" data
stats.all <- data.frame(greenup= stats.greenup,
                        midgreenup= stats.midgreenup, 
                        alba= stats.alba, bur= stats.bur,
                        stringsAsFactors=FALSE)
summary(stats.all)

# We want "long" data
stats.macro <- stats.bur

# What pieces of information do we need to distinguish
stats.greenup$name <- "greenup"
stats.midgreenup$name <- "midgreenup"
stats.alba$name <- "Q. alba"
stats.macro$name <- "Q. macrocarpa"

stats.greenup$type <- "MODIS"
stats.midgreenup$type <- "MODIS"
stats.alba$type <- "NPN"
stats.macro$type <- "NPN"

# Combine our different data frames into "long" format 
dat.all <- rbind(stats.greenup, stats.midgreenup, stats.alba, stats.macro)
dat.all$name <- as.factor(dat.all$name)
dat.all$type <- as.factor(dat.all$type)
summary(dat.all)

ggplot(data= dat.all) +
  # facet_wrap(~type) + # breaks things into separate panels
  ggtitle('Comparison of Growing Degree Day Thresholds at The Morton Arboretum') +
  geom_density(mapping = aes(x= THRESH, color = name, fill=name), alpha=0.5) +
  scale_color_manual(values=c("darkblue", "lightblue", "darkgreen", "lightgreen")) +
  scale_fill_manual(values=c("darkblue", "lightblue", "darkgreen", "lightgreen"))

#graph all the densities together to see overlap.
figures.dat <- '../figures'
if(!dir.exists(figures.dat)) dir.create(figures.dat)
png(width= 750, filename= file.path(figures.dat, 'THRESH_FIG_MortonArb.png'))
    
ggplot(data= stats.all) +
  ggtitle('Comparison of Growing Degree Day Thresholds at The Morton Arboretum') +
  geom_density(mapping = aes(x= greenup.THRESH, color = 'Greenup')) +
  geom_density(mapping = aes(x= midgreenup.THRESH, color = 'Midgreenup')) +
  geom_density(mapping = aes(x= alba.THRESH, color = 'Alba')) +
  geom_density(mapping = aes(x= bur.THRESH, color = 'Bur')) +
  scale_x_continuous('THRESH (5C GDD)', breaks= seq(from= '-110', to='300', by= 20)) +
  scale_y_continuous('DENSITY (%)')
dev.off()

# save the outputs
path.mod.out <- "../data_processed/mod.gdd5.MortonArb"
if(!dir.exists(path.mod.out)) dir.create(path.mod.out)
write.csv(stats.greenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_Greenup.csv"), row.names=F)
write.csv(stats.midgreenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_MidGreenup.csv"), row.names=F)
#resolve
write.csv(stats.bur, file.path(path.mod.out, "THRESH_GDD5_Bur.csv"), row.names=F) 


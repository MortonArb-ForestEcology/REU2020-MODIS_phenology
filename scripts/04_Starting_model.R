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

#THIS IS WHERE YOU DO THINGS
dat.processed <- "../data_processed/"
site.id <- "MortonArb"

#Here is the different data to compare. All are constrained to GDD5.cum
#------------------------------
#MODIS Threshold Estimates for 15% greenup <- (greenup) and 50% greenup <- (midgreenup)
dat.MODIS <- read.csv(file.path(dat.processed, paste0("MODIS_MET_", site.id, ".csv")))
head(dat.MODIS)

#-----------------------------
#NPN Threshold Estimate
oak.budburst <- read.csv(file.path(dat.processed, paste0("Quercus_bud", site.id, "_NPN_MET.csv")))
head(oak.budburst)

oak.leaves <- read.csv(file.path(dat.processed, paste0("Quercus_leaf", site.id, "_NPN_MET.csv")))
head(oak.budburst)

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
#This section converts our observed into the necessary format, defines our uninformed prior, and sets up our MCMC chains
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

green.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum']))

midgreen.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum']))

#looking at the species available in the NPN data
unique(oak.budburst$species)

unique(oak.leaves$species)

#bud burst lists
bud.mont.list <- list(y = oak.budburst[oak.budburst$species == 'montana', 'GDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'montana', 'GDD5.cum']))
bud.alba.list <- list(y = oak.budburst[oak.budburst$species == 'alba', 'GDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'alba', 'GDD5.cum']))
bud.gamb.list <- list(y = oak.budburst[oak.budburst$species == 'gambelii', 'GDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'gambelii', 'GDD5.cum']))
bud.rubr.list <- list(y = oak.budburst[oak.budburst$species == 'rubra', 'GDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'rubra', 'GDD5.cum']))

#leaves list
leaf.mont.list <- list(y = oak.leaves[oak.leaves$species == 'montana', 'GDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'montana', 'GDD5.cum']))
leaf.alba.list <- list(y = oak.leaves[oak.leaves$species == 'alba', 'GDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'alba', 'GDD5.cum']))
leaf.gamb.list <- list(y = oak.leaves[oak.leaves$species == 'gambelii', 'GDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'gambelii', 'GDD5.cum']))
leaf.rubr.list <- list(y = oak.leaves[oak.leaves$species == 'rubra', 'GDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'rubra', 'GDD5.cum']))

#running the model
#good oaks = rubra 9, gambelii 13, shumardii 7, montana 10, alba 9, macrocarpa 7, velutina 10, imbricaria 11, 
#bad oaks = palustris, lobata, phellos 3, ilicifolia 6

green.mod   <- jags.model (file = textConnection(univariate_regression),
                             data = green.list,
                             inits = inits,
                             n.chains = 3)
midgreen.mod <- jags.model (file = textConnection(univariate_regression),
                            data = midgreen.list,
                            inits = inits,
                            n.chains = 3)
bud.mont.mod <- jags.model (file = textConnection(univariate_regression),
                       data = bud.mont.list,
                       inits = inits,
                       n.chains = 3)
bud.alba.mod <- jags.model (file = textConnection(univariate_regression),
                           data = bud.alba.list,
                           inits = inits,
                           n.chains = 3)
bud.gamb.mod <- jags.model (file = textConnection(univariate_regression),
                        data = bud.gamb.list,
                        inits = inits,
                        n.chains = 3)
bud.rubr.mod <- jags.model (file = textConnection(univariate_regression),
                        data = bud.rubr.list,
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
bud.mont.out   <- coda.samples (model = bud.mont.mod,
                             variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
bud.alba.out   <- coda.samples (model = bud.alba.mod,
                            variable.names = c("THRESH", "Prec"),
                            n.iter = 5000)
bud.gamb.out   <- coda.samples (model = bud.gamb.mod,
                            variable.names = c("THRESH", "Prec"),
                            n.iter = 5000)
bud.rubr.out   <- coda.samples (model = bud.rubr.mod,
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
plot(bud.mont.out)
summary(bud.mont.out)

plot(bud.alba.out)
summary(bud.alba.out)

plot(bud.gamb.out)
summary(bud.gamb.out)

plot(bud.rubr.out)
summary(bud.rubr.out)


#Checking that convergence happened
#1 is ideal, below 1.05 is fine
gelman.diag(green.out)
gelman.diag(midgreen.out)
gelman.diag(bud.mont.out)
gelman.diag(bud.alba.out)
gelman.diag(bud.gamb.out)
gelman.diag(bud.rubr.out)
#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
green.burn <- window(green.out, start= burnin)  ## remove burn-in
midgreen.burn <- window(midgreen.out, start= burnin)
bud.alba.burn <- window(bud.alba.out, start= burnin)
bud.mont.burn <- window(bud.mont.out, start= burnin)
bud.gamb.burn <- window(bud.gamb.out, start= burnin)
bud.rubr.burn <- window(bud.rubr.out, start= burnin)

# save the part of the stats from when the model worked and converged
stats.greenup <- as.data.frame(as.matrix(green.burn))
summary(stats.greenup)


stats.midgreenup <- as.data.frame(as.matrix(midgreen.burn))
summary(stats.midgreenup)


bud.stats.alba <- as.data.frame(as.matrix(bud.alba.burn))
summary(bud.stats.alba)


bud.stats.mont <- as.data.frame(as.matrix(bud.mont.burn))
summary(bud.stats.mont)


bud.stats.gamb <- as.data.frame(as.matrix(bud.gamb.burn))
summary(bud.stats.gamb)


bud.stats.rubr <- as.data.frame(as.matrix(bud.rubr.burn))
summary(bud.stats.rubr)



# What pieces of information do we need to distinguish?
stats.greenup$name <- "greenup"
stats.midgreenup$name <- "midgreenup"
bud.stats.alba$name <- "Q. alba"
bud.stats.rubr$name <- "Q. rubra"
bud.stats.mont$name <- "Q. montana"
bud.stats.gamb$name <- "Q. gambelii"

stats.greenup$type <- "MODIS"
stats.midgreenup$type <- "MODIS"
bud.stats.alba$type <- "NPN"
bud.stats.mont$type <- "NPN"
bud.stats.gamb$type <- "NPN"
bud.stats.rubr$type <- "NPN"

# Combine our different data frames into "long" format 
bud.dat.all <- rbind(stats.greenup, stats.midgreenup, bud.stats.alba, bud.stats.mont, bud.stats.gamb, bud.stats.rubr)
bud.dat.all$name <- as.factor(bud.dat.all$name)
bud.dat.all$type <- as.factor(bud.dat.all$type)
summary(bud.dat.all)

library(ggplot2)
figures.dat <- '../figures'
if(!dir.exists(figures.dat)) dir.create(figures.dat)
png(width= 750, filename= file.path(figures.dat, 'THRESH_Oaks_MortonArb.png'))

ggplot(data= bud.dat.all) +
  # facet_wrap(~type) + # breaks things into separate panels
  ggtitle('Thermal Time Thresholds at Average Bud Burst Onset of 4 Quercus at The Morton Arboretum') +
  geom_density(mapping = aes(x= THRESH, color = name, fill=name), alpha=0.5) +
  scale_color_manual(values=c("darkblue", "lightblue", "olivedrab", "olivedrab1", 'olivedrab2','olivedrab3')) +
  scale_fill_manual(values=c("blue", "dodgerblue", "olivedrab", "olivedrab1", 'olivedrab2','olivedrab3')) +
  scale_x_continuous('THRESH (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
#dev.off()

# save the outputs
path.mod.out <- "../data_processed/mod.gdd5.MortonArb"
if(!dir.exists(path.mod.out)) dir.create(path.mod.out)
write.csv(stats.greenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_Greenup.csv"), row.names=F)
write.csv(stats.midgreenup, file.path(path.mod.out, "THRESH_GDD5_MODIS_MidGreenup.csv"), row.names=F)
write.csv(bud.stats.alba, file.path(path.mod.out, "THRESH_bud_alba.csv"), row.names=F) 
write.csv(bud.stats.mont, file.path(path.mod.out, "THRESH_bud_montana.csv"), row.names=F) 
write.csv(bud.stats.rubr, file.path(path.mod.out, "THRESH_bud_rubra.csv"), row.names=F) 
write.csv(bud.stats.gamb, file.path(path.mod.out, "THRESH_bud_gambelii.csv"), row.names=F) 


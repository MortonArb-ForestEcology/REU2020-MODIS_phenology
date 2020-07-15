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
#!!!! Specifically for "Breaking Leaf Buds" Phenophase and MODIS!!!!
#-----------------------------------------------------------------------------------------------------------------------------------#
site.id <- "MortonArb"

#THIS IS WHERE YOU DO THINGS

#Here is the different data to compare. All are constrained to GDD5.cum
#------------------------------
#MODIS Threshold Estimates for 15% greenup <- (greenup) and 50% greenup <- (midgreenup)
dat.processed <- file.path("../data_processed/MODIS")
if(!dir.exists(dat.processed)) dir.create(dat.processed)
dat.MODIS <- read.csv(file.path(dat.processed, paste0("MODIS_MET_GDD5", site.id, ".csv")))
head(dat.MODIS)

#-----------------------------
#NPN Threshold Estimate
path.clean <- "../data_processed/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)

oak.budburst <- read.csv(file.path(path.clean, paste0("Quercus_bud_GDD5", site.id, "_NPN.csv")))
head(oak.budburst)
summary(oak.budburst)
oak.budburst$species <- as.factor(oak.budburst$species)
summary(oak.budburst[!is.na(oak.budburst$first.mean), 'species'])
#not using phellos and lobata, too few observaitons

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

#MODIS lists for grenup and midgreenup
green.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum']))

midgreen.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum']))

unique(oak.budburst$species) #choosing to exclude phellos and lobata

#bud burst lists for dat.budburst$MeanGDD5.cum
bud.ilic.list <- list(y = oak.budburst[oak.budburst$species == 'ilicifolia', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'ilicifolia', 'MeanGDD5.cum']))
bud.imbr.list <- list(y = oak.budburst[oak.budburst$species == 'imbricaria', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'imbricaria', 'MeanGDD5.cum']))
bud.macr.list <- list(y = oak.budburst[oak.budburst$species == 'macrocarpa', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'macrocarpa', 'MeanGDD5.cum']))
bud.palu.list <- list(y = oak.budburst[oak.budburst$species == 'palustris', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'palustris', 'MeanGDD5.cum']))
bud.shum.list <- list(y = oak.budburst[oak.budburst$species == 'shumardii', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'shumardii', 'MeanGDD5.cum']))
bud.velu.list <- list(y = oak.budburst[oak.budburst$species == 'velutina', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'velutina', 'MeanGDD5.cum']))
bud.gamb.list <- list(y = oak.budburst[oak.budburst$species == 'gambelii', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'gambelii', 'MeanGDD5.cum']))
bud.mont.list <- list(y = oak.budburst[oak.budburst$species == 'montana', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'montana', 'MeanGDD5.cum']))
bud.rubr.list <- list(y = oak.budburst[oak.budburst$species == 'rubra', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'rubra', 'MeanGDD5.cum']))
bud.alba.list <- list(y = oak.budburst[oak.budburst$species == 'alba', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'alba', 'MeanGDD5.cum']))

#running the model

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
bud.ilic.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.ilic.list,
                            inits = inits,
                            n.chains = 3)
bud.imbr.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.imbr.list,
                            inits = inits,
                            n.chains = 3)
bud.macr.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.macr.list,
                            inits = inits,
                            n.chains = 3)
bud.palu.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.palu.list,
                            inits = inits,
                            n.chains = 3)
bud.shum.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.shum.list,
                            inits = inits,
                            n.chains = 3)
bud.velu.mod <- jags.model (file = textConnection(univariate_regression),
                            data = bud.velu.list,
                            inits = inits,
                            n.chains = 3)

#Converting the output into a workable format
#DO THINGS HERE SOMETIMES
green.out <- coda.samples (model = green.mod,
                            variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
midgreen.out <- coda.samples (model = midgreen.mod,
                            variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
bud.mont.out <- coda.samples (model = bud.mont.mod,
                            variable.names = c("THRESH", "Prec"),
                             n.iter = 5000)
bud.alba.out <- coda.samples (model = bud.alba.mod,
                            variable.names = c("THRESH", "Prec"),
                            n.iter = 5000)
bud.gamb.out <- coda.samples (model = bud.gamb.mod,
                            variable.names = c("THRESH", "Prec"),
                            n.iter = 5000)
bud.rubr.out <- coda.samples (model = bud.rubr.mod,
                            variable.names = c("THRESH", "Prec"),
                            n.iter = 5000)
bud.ilic.out <- coda.samples (model = bud.ilic.mod,
                            variable.names = c("THRESH", "Prec"),
                                n.iter = 5000)
bud.imbr.out <- coda.samples (model = bud.imbr.mod,
                            variable.names = c("THRESH", "Prec"),
                                n.iter = 5000)
bud.macr.out <- coda.samples (model = bud.macr.mod,
                            variable.names = c("THRESH", "Prec"),
                                n.iter = 5000)
bud.palu.out <- coda.samples (model = bud.palu.mod,
                            variable.names = c("THRESH", "Prec"),
                                n.iter = 5000)
bud.shum.out <- coda.samples (model = bud.shum.mod,
                            variable.names = c("THRESH", "Prec"),
                                n.iter = 5000)
bud.velu.out <- coda.samples (model = bud.velu.mod,
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

#For NPN.gdd5
plot(bud.mont.out)
summary(bud.mont.out)

plot(bud.alba.out)
summary(bud.alba.out)

plot(bud.gamb.out)
summary(bud.gamb.out)

plot(bud.rubr.out)
summary(bud.rubr.out)

plot(bud.ilic.out)
summary(bud.ilic.out)

plot(bud.imbr.out)
summary(bud.imbr.out)

plot(bud.macr.out)
summary(bud.macr.out)

plot(bud.palu.out)
summary(bud.palu.out)

plot(bud.shum.out)
summary(bud.shum.out)

plot(bud.velu.out)
summary(bud.velu.out)

#Checking that convergence happened
#1 is ideal, below 1.05 is fine
gelman.diag(green.out)
gelman.diag(midgreen.out)
gelman.diag(bud.mont.out)
gelman.diag(bud.alba.out)
gelman.diag(bud.gamb.out)
gelman.diag(bud.rubr.out)
gelman.diag(bud.ilic.out)
gelman.diag(bud.imbr.out)
gelman.diag(bud.macr.out)
gelman.diag(bud.palu.out)
gelman.diag(bud.shum.out)
gelman.diag(bud.velu.out)

#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
green.burn <- window(green.out, start= burnin)  ## remove burn-in
midgreen.burn <- window(midgreen.out, start= burnin)
bud.alba.burn <- window(bud.alba.out, start= burnin)
bud.mont.burn <- window(bud.mont.out, start= burnin)
bud.gamb.burn <- window(bud.gamb.out, start= burnin)
bud.rubr.burn <- window(bud.rubr.out, start= burnin)
bud.ilic.burn <- window(bud.ilic.out, start= burnin)
bud.imbr.burn <- window(bud.imbr.out, start= burnin)
bud.macr.burn <- window(bud.macr.out, start= burnin)
bud.palu.burn <- window(bud.palu.out, start= burnin)
bud.shum.burn <- window(bud.shum.out, start= burnin)
bud.velu.burn <- window(bud.velu.out, start= burnin)

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

bud.stats.ilic <- as.data.frame(as.matrix(bud.ilic.burn))
summary(bud.stats.ilic)

bud.stats.imbr <- as.data.frame(as.matrix(bud.imbr.burn))
summary(bud.stats.imbr)

bud.stats.macr <- as.data.frame(as.matrix(bud.macr.burn))
summary(bud.stats.macr)

bud.stats.palu <- as.data.frame(as.matrix(bud.palu.burn))
summary(bud.stats.palu)

bud.stats.shum <- as.data.frame(as.matrix(bud.shum.burn))
summary(bud.stats.shum)

bud.stats.velu <- as.data.frame(as.matrix(bud.velu.burn))
summary(bud.stats.velu)

# What pieces of information do we need to distinguish?
stats.greenup$name <- "greenup"
stats.midgreenup$name <- "midgreenup"
bud.stats.alba$name <- "Q. alba"
bud.stats.rubr$name <- "Q. rubra"
bud.stats.mont$name <- "Q. montana"
bud.stats.gamb$name <- "Q. gambelii"
bud.stats.ilic$name <- "Q. ilicifolia"
bud.stats.imbr$name <- "Q. imbricaria"
bud.stats.macr$name <- "Q. macrocarpa"
bud.stats.palu$name <- "Q. palustris"
bud.stats.shum$name <- "Q. shumardii"
bud.stats.velu$name <- "Q. velutina"

stats.greenup$type <- "MODIS"
stats.midgreenup$type <- "MODIS"
bud.stats.alba$type <- "NPN"
bud.stats.mont$type <- "NPN"
bud.stats.gamb$type <- "NPN"
bud.stats.rubr$type <- "NPN"
bud.stats.ilic$type <- "NPN"
bud.stats.imbr$type <- "NPN"
bud.stats.macr$type <- "NPN"
bud.stats.palu$type <- "NPN"
bud.stats.shum$type <- "NPN"
bud.stats.velu$type <- "NPN"

# Combine our different data frames into "long" format 
bud.first.mean <- rbind(stats.greenup, stats.midgreenup, bud.stats.alba, bud.stats.mont, bud.stats.gamb, bud.stats.rubr, bud.stats.ilic, bud.stats.imbr
                     ,bud.stats.macr, bud.stats.palu, bud.stats.shum, bud.stats.velu)
bud.first.mean$name <- as.factor(bud.first.mean$name)
bud.first.mean$type <- as.factor(bud.first.mean$type)
summary(bud.first.mean)

library(ggplot2)
figures.dat <- '../figures'
if(!dir.exists(figures.dat)) dir.create(figures.dat)
png(width= 750, filename= file.path(figures.dat, 'THRESH_bud_firstmean_MortonArb.png'))

ggplot(data= bud.first.mean) +
  ggtitle('Thermal Time Thresholds at First Mean Bud Burst Onset of Quercus at The Morton Arboretum') +
  geom_density(mapping = aes(x= THRESH, color = name, fill=name), alpha=0.5) +
  scale_color_manual(values=c("darkblue", "lightblue", "mediumspringgreen", "olivedrab2", 'goldenrod1','darkolivegreen4', 'goldenrod3', 'aquamarine3'
                              , 'olivedrab4', 'chartreuse3', 'forestgreen', 'lightgreen')) +
  scale_fill_manual(values=c("darkblue", "lightblue", "mediumspringgreen", "olivedrab2", 'goldenrod1','darkolivegreen4', 'goldenrod3', 'aquamarine3'
                             , 'olivedrab4', 'chartreuse3', 'forestgreen', 'lightgreen'))  +
  scale_x_continuous('Thermal Time Threshold Mean (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
dev.off()

# save the outputs
path.mod.firstmean <- "../data_processed/mod.firstmean.MortonArb"
if(!dir.exists(path.mod.firstmean)) dir.create(path.mod.firstmean)
write.csv(stats.greenup, file.path(path.mod.firstmean, "THRESH_MODIS_Greenup.csv"), row.names=F)
write.csv(stats.midgreenup, file.path(path.mod.firstmean, "THRESH_MODIS_MidGreenup.csv"), row.names=F)
write.csv(bud.stats.alba, file.path(path.mod.firstmean, "THRESH_bud_firstmean_alba.csv"), row.names=F) 
write.csv(bud.stats.mont, file.path(path.mod.firstmean, "THRESH_bud_firstmean_montana.csv"), row.names=F) 
write.csv(bud.stats.gamb, file.path(path.mod.firstmean, "THRESH_bud_firstmean_gambelii.csv"), row.names=F) 
write.csv(bud.stats.rubr, file.path(path.mod.firstmean, "THRESH_bud_firstmean_rubra.csv"), row.names=F) 
write.csv(bud.stats.ilic, file.path(path.mod.firstmean, "THRESH_bud_firstmean_ilicifolia.csv"), row.names=F) 
write.csv(bud.stats.imbr, file.path(path.mod.firstmean, "THRESH_bud_firstmean_imbricaria.csv"), row.names=F) 
write.csv(bud.stats.macr, file.path(path.mod.firstmean, "THRESH_bud_firstmean_macrocarpa.csv"), row.names=F) 
write.csv(bud.stats.palu, file.path(path.mod.firstmean, "THRESH_bud_firstmean_palustris.csv"), row.names=F) 
write.csv(bud.stats.shum, file.path(path.mod.firstmean, "THRESH_bud_firstmean_shumardii.csv"), row.names=F) 
write.csv(bud.stats.velu, file.path(path.mod.firstmean, "THRESH_bud_firstmean_velutina.csv"), row.names=F) 


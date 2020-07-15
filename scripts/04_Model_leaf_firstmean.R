#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of leaf out timing
#          This script serves as the basic Bayesian model which will be built off of
# Inputs: dat.comb dataframe that is created by the Organize_Data_Pheno.R script
# Outputs: Model Prediciton of the GDD5 threshold
# Notes: This script is based on exercises from the ecological forecasting textbook
#        In order to use rjags you need JAGS installed. rjags is simply for interfacing. It can be found at http://mcmc-jags.sourceforge.net/

#-----------------------------------------------------------------------------------------------------------------------------------#
#!!!! Specifically for "Leaves" Phenophase and MODIS !!!!
#-----------------------------------------------------------------------------------------------------------------------------------#

site.id <- "MortonArb"

#Here is the different data to compare. All are constrained to GDD5.cum
#------------------------------
#MODIS Threshold Estimates for 15% greenup <- (greenup) and 50% greenup <- (midgreenup)
dat.processed <- file.path("../data_processed/MODIS")
if(!dir.exists(dat.processed)) dir.create(dat.processed)
dat.MODIS <- read.csv(file.path(dat.processed, paste0("MODIS_MET_GDD5_", site.id, ".csv")))
head(dat.MODIS)


#-----------------------------
#NPN Threshold Estimate
path.cleaned <- "../data_processed/NPN"
if(!dir.exists(path.cleaned)) dir.create(path.cleaned)
oak.leaves <- read.csv(file.path(path.cleaned, paste0("Quercus_leaf_GDD5_", site.id, "_NPN.csv")))
head(oak.leaves)
oak.leaves$species <- as.factor(oak.leaves$species)
summary(oak.leaves[!is.na(oak.leaves$first.mean), 'species'])
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

#leaves list for dat.leaves$MeanGDD5.cum
Meanleaf.ilic.list <- list(y = oak.leaves[oak.leaves$species == 'ilicifolia', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'ilicifolia', 'MeanGDD5.cum']))
Meanleaf.imbr.list <- list(y = oak.leaves[oak.leaves$species == 'imbricaria', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'imbricaria', 'MeanGDD5.cum']))
Meanleaf.macr.list <- list(y = oak.leaves[oak.leaves$species == 'macrocarpa', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'macrocarpa', 'MeanGDD5.cum']))
Meanleaf.palu.list <- list(y = oak.leaves[oak.leaves$species == 'palustris', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'palustris', 'MeanGDD5.cum']))
Meanleaf.shum.list <- list(y = oak.leaves[oak.leaves$species == 'shumardii', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'shumardii', 'MeanGDD5.cum']))
Meanleaf.velu.list <- list(y = oak.leaves[oak.leaves$species == 'velutina', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'veultina', 'MeanGDD5.cum']))
Meanleaf.gamb.list <- list(y = oak.leaves[oak.leaves$species == 'gambelii', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'gambelii', 'MeanGDD5.cum']))
Meanleaf.mont.list <- list(y = oak.leaves[oak.leaves$species == 'montana', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'montana', 'MeanGDD5.cum']))
Meanleaf.rubr.list <- list(y = oak.leaves[oak.leaves$species == 'rubra', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'rubra', 'MeanGDD5.cum']))
Meanleaf.alba.list <- list(y = oak.leaves[oak.leaves$species == 'alba', 'MeanGDD5.cum'], n = length(oak.leaves[oak.leaves$species== 'alba', 'MeanGDD5.cum']))

#running the model

green.mod   <- jags.model (file = textConnection(univariate_regression),
                           data = green.list,
                           inits = inits,
                           n.chains = 3)
midgreen.mod <- jags.model (file = textConnection(univariate_regression),
                            data = midgreen.list,
                            inits = inits,
                            n.chains = 3)
leaf.mont.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.mont.list,
                             inits = inits,
                             n.chains = 3)
leaf.alba.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.alba.list,
                             inits = inits,
                             n.chains = 3)
leaf.gamb.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.gamb.list,
                             inits = inits,
                             n.chains = 3)
leaf.rubr.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.rubr.list,
                             inits = inits,
                             n.chains = 3)
leaf.ilic.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.ilic.list,
                             inits = inits,
                             n.chains = 3)
leaf.imbr.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.imbr.list,
                             inits = inits,
                             n.chains = 3)
leaf.macr.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.macr.list,
                             inits = inits,
                             n.chains = 3)
leaf.palu.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.palu.list,
                             inits = inits,
                             n.chains = 3)
leaf.shum.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.shum.list,
                             inits = inits,
                             n.chains = 3)
leaf.velu.mod <- jags.model (file = textConnection(univariate_regression),
                             data = Meanleaf.velu.list,
                             inits = inits,
                             n.chains = 3)

#Converting the output into a workable format

green.out <- coda.samples (model = green.mod,
                           variable.names = c("THRESH", "Prec"),
                           n.iter = 5000)
midgreen.out <- coda.samples (model = midgreen.mod,
                              variable.names = c("THRESH", "Prec"),
                              n.iter = 5000)
leaf.mont.out <- coda.samples (model = leaf.mont.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.alba.out <- coda.samples (model = leaf.alba.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.gamb.out <- coda.samples (model = leaf.gamb.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.rubr.out <- coda.samples (model = leaf.rubr.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.ilic.out <- coda.samples (model = leaf.ilic.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.imbr.out <- coda.samples (model = leaf.imbr.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.macr.out <- coda.samples (model = leaf.macr.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.palu.out <- coda.samples (model = leaf.palu.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.shum.out <- coda.samples (model = leaf.shum.mod,
                               variable.names = c("THRESH", "Prec"),
                               n.iter = 5000)
leaf.velu.out <- coda.samples (model = leaf.velu.mod,
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
plot(leaf.mont.out)
summary(leaf.mont.out)

plot(leaf.alba.out)
summary(leaf.alba.out)

plot(leaf.gamb.out)
summary(leaf.gamb.out)

plot(leaf.rubr.out)
summary(leaf.rubr.out)

plot(leaf.ilic.out)
summary(leaf.ilic.out)

plot(leaf.imbr.out)
summary(leaf.imbr.out)

plot(leaf.macr.out)
summary(leaf.macr.out)

plot(leaf.palu.out)
summary(leaf.palu.out)

plot(leaf.shum.out)
summary(leaf.shum.out)

plot(leaf.velu.out)
summary(leaf.velu.out)

#Checking that convergence happened
#1 is ideal, below 1.05 is fine
gelman.diag(green.out)
gelman.diag(midgreen.out)
gelman.diag(leaf.mont.out)
gelman.diag(leaf.alba.out)
gelman.diag(leaf.gamb.out)
gelman.diag(leaf.rubr.out)
gelman.diag(leaf.ilic.out)
gelman.diag(leaf.imbr.out)
gelman.diag(leaf.macr.out)
gelman.diag(leaf.palu.out)
gelman.diag(leaf.shum.out)
gelman.diag(leaf.velu.out)

#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 1000                                ## determine convergence from GBR output
green.burn <- window(green.out, start= burnin)  ## remove burn-in
midgreen.burn <- window(midgreen.out, start= burnin)
leaf.alba.burn <- window(leaf.alba.out, start= burnin)
leaf.mont.burn <- window(leaf.mont.out, start= burnin)
leaf.gamb.burn <- window(leaf.gamb.out, start= burnin)
leaf.rubr.burn <- window(leaf.rubr.out, start= burnin)
leaf.ilic.burn <- window(leaf.ilic.out, start= burnin)
leaf.imbr.burn <- window(leaf.imbr.out, start= burnin)
leaf.macr.burn <- window(leaf.macr.out, start= burnin)
leaf.palu.burn <- window(leaf.palu.out, start= burnin)
leaf.shum.burn <- window(leaf.shum.out, start= burnin)
leaf.velu.burn <- window(leaf.velu.out, start= burnin)

# save the part of the stats from when the model worked and converged
stats.greenup <- as.data.frame(as.matrix(green.burn))
summary(stats.greenup)

stats.midgreenup <- as.data.frame(as.matrix(midgreen.burn))
summary(stats.midgreenup)

leaf.stats.alba <- as.data.frame(as.matrix(leaf.alba.burn))
summary(leaf.stats.alba)

leaf.stats.mont <- as.data.frame(as.matrix(leaf.mont.burn))
summary(leaf.stats.mont)

leaf.stats.gamb <- as.data.frame(as.matrix(leaf.gamb.burn))
summary(leaf.stats.gamb)

leaf.stats.rubr <- as.data.frame(as.matrix(leaf.rubr.burn))
summary(leaf.stats.rubr)

leaf.stats.ilic <- as.data.frame(as.matrix(leaf.ilic.burn))
summary(leaf.stats.ilic)

leaf.stats.imbr <- as.data.frame(as.matrix(leaf.imbr.burn))
summary(leaf.stats.imbr)

leaf.stats.macr <- as.data.frame(as.matrix(leaf.macr.burn))
summary(leaf.stats.macr)

leaf.stats.palu <- as.data.frame(as.matrix(leaf.palu.burn))
summary(leaf.stats.palu)

leaf.stats.shum <- as.data.frame(as.matrix(leaf.shum.burn))
summary(leaf.stats.shum)

leaf.stats.velu <- as.data.frame(as.matrix(leaf.velu.burn))
summary(leaf.stats.velu)

# What pieces of information do we need to distinguish?
stats.greenup$name <- "greenup"
stats.midgreenup$name <- "midgreenup"
leaf.stats.alba$name <- "Q. alba"
leaf.stats.rubr$name <- "Q. rubra"
leaf.stats.mont$name <- "Q. montana"
leaf.stats.gamb$name <- "Q. gambelii"
leaf.stats.ilic$name <- "Q. ilicifolia"
leaf.stats.imbr$name <- "Q. imbricaria"
leaf.stats.macr$name <- "Q. macrocarpa"
leaf.stats.palu$name <- "Q. palustris"
leaf.stats.shum$name <- "Q. shumardii"
leaf.stats.velu$name <- "Q. velutina"

stats.greenup$type <- "MODIS"
stats.midgreenup$type <- "MODIS"
leaf.stats.alba$type <- "NPN"
leaf.stats.mont$type <- "NPN"
leaf.stats.gamb$type <- "NPN"
leaf.stats.rubr$type <- "NPN"
leaf.stats.ilic$type <- "NPN"
leaf.stats.imbr$type <- "NPN"
leaf.stats.macr$type <- "NPN"
leaf.stats.palu$type <- "NPN"
leaf.stats.shum$type <- "NPN"
leaf.stats.velu$type <- "NPN"

# Combine our different data frames into "long" format 
leaf.first.mean <- rbind(stats.greenup, stats.midgreenup, leaf.stats.alba, leaf.stats.mont, leaf.stats.gamb, leaf.stats.rubr, leaf.stats.ilic, leaf.stats.imbr
                        ,leaf.stats.macr, leaf.stats.palu, leaf.stats.shum, leaf.stats.velu)
leaf.first.mean$name <- as.factor(leaf.first.mean$name)
leaf.first.mean$type <- as.factor(leaf.first.mean$type)
summary(leaf.first.mean)

library(ggplot2)
path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Thresh_leaf_firstmean_MortonArb.png')))

ggplot(data= leaf.first.mean) +
  ggtitle('Thermal Time Thresholds at First Mean leaf Onset of Quercus at The Morton Arboretum') +
  geom_density(mapping = aes(x= THRESH, color = name, fill=name), alpha=0.5) +
  scale_color_manual(values=c("darkblue", "lightblue", "mediumspringgreen", "olivedrab2", 'goldenrod1','darkolivegreen4', 'goldenrod3', 'aquamarine3'
                              , 'olivedrab4', 'chartreuse3', 'forestgreen', 'lightgreen')) +
  scale_fill_manual(values=c("darkblue", "lightblue", "mediumspringgreen", "olivedrab2", 'goldenrod1','darkolivegreen4', 'goldenrod3', 'aquamarine3'
                             , 'olivedrab4', 'chartreuse3', 'forestgreen', 'lightgreen'))  +
  scale_x_continuous('Thermal Time Threshold Mean (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
dev.off()

# save the outputs
path.mod.leaf.firstmean <- "../data_processed/leaf.firstmean.MortonArb"
if(!dir.exists(path.mod.leaf.firstmean)) dir.create(path.mod.leaf.firstmean)
write.csv(stats.greenup, file.path(path.mod.leaf.firstmean, "THRESH_MODIS_Greenup.csv"), row.names=F)
write.csv(stats.midgreenup, file.path(path.mod.leaf.firstmean, "THRESH_MODIS_MidGreenup.csv"), row.names=F)
write.csv(leaf.stats.alba, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_alba.csv"), row.names=F) 
write.csv(leaf.stats.mont, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_montana.csv"), row.names=F) 
write.csv(leaf.stats.gamb, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_gambelii.csv"), row.names=F) 
write.csv(leaf.stats.rubr, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_rubra.csv"), row.names=F) 
write.csv(leaf.stats.ilic, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_ilicifolia.csv"), row.names=F) 
write.csv(leaf.stats.imbr, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_imbricaria.csv"), row.names=F) 
write.csv(leaf.stats.macr, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_macrocarpa.csv"), row.names=F) 
write.csv(leaf.stats.palu, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_palustris.csv"), row.names=F) 
write.csv(leaf.stats.shum, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_shumardii.csv"), row.names=F) 
write.csv(leaf.stats.velu, file.path(path.mod.leaf.firstmean, "THRESH_leaf_firstmean_velutina.csv"), row.names=F) 


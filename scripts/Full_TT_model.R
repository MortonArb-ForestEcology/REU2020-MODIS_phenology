#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHER YOU DO THINGS
dat.processed <- "../data_processed/"
site.id <- "MortonArb"

dat.MODIS <- read.csv(file.path(dat.processed, paste0("MODIS_MET_", site.id, ".csv")))
head(dat.MODIS)

#-----------------------------
#NPN Threshold Estimate
oak.budburst <- read.csv(file.path(dat.processed, paste0("Quercus_bud", site.id, "_NPN_MET.csv")))
head(oak.budburst)
summary(oak.budburst)
oak.budburst$species <- as.factor(oak.budburst$species)
summary(oak.budburst[!is.na(oak.budburst$first.mean), 'species'])


NPN_regression <- "
  model{
    
    for(k in 1:n){
      mu[k] <- THRESH[loc[k]]  #Combination of species Threshold and individual effect
      y[k] ~ dnorm(mu[k], sPrec)
    }

    for(t in 1:nLoc){
    THRESH[t] <-  c[t] + ind[pln[t]]
    c[t] ~ dnorm(0, aPrec[t])
    aPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    bPrec ~ dgamma(0.1, 0.1)
    sPrec ~ dgamma(0.1, 0.1)
  }
  "

nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(Prec = runif(1,1/200,1/20))
}


#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
#Converting to list format needed for JAGs

#MODIS lists for grenup and midgreenup
#green.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'Greenup', 'GDD5.cum']))

#midgreen.list <- list(y = dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum'], n = length(dat.MODIS[dat.MODIS$BAND== 'MidGreenup', 'GDD5.cum']))

unique(oak.budburst$species) #choosing to exclude phellos and lobata

#bud burst lists for dat.budburst$MeanGDD5.cum
bud.ilic.list <- list(y = oak.budburst[oak.budburst$species == 'ilicifolia', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'ilicifolia', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'ilicifolia', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'ilicifolia', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'ilicifolia', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'ilicifolia', 'individual_id'])))

bud.macr.list <- list(y = oak.budburst[oak.budburst$species == 'macrocarpa', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'macrocarpa', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'macrocarpa', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'macrocarpa', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'macrocarpa', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'macrocarpa', 'individual_id'])))

bud.imbr.list <- list(y = oak.budburst[oak.budburst$species == 'imbricaria', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'imbricaria', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'imbricaria', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'imbricaria', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'imbricaria', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'imbricaria', 'individual_id'])))

bud.palu.list <- list(y = oak.budburst[oak.budburst$species == 'palustris', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'palustris', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'palustris', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'palustris', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'palustris', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'palustris', 'individual_id'])))

bud.shum.list <- list(y = oak.budburst[oak.budburst$species == 'shumardii', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'shumardii', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'shumardii', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'shumardii', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'shumardii', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'shumardii', 'individual_id'])))

bud.velu.list <- list(y = oak.budburst[oak.budburst$species == 'velutina', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'velutina', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'velutina', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'velutina', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'velutina', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'velutina', 'individual_id'])))

bud.gamb.list <- list(y = oak.budburst[oak.budburst$species == 'gambelii', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'gambelii', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'gambelii', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'gambelii', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'gambelii', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'gambelii', 'individual_id'])))

bud.mont.list <- list(y = oak.budburst[oak.budburst$species == 'montana', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'montana', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'montana', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'montana', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'montana', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'montana', 'individual_id'])))

bud.rubr.list <- list(y = oak.budburst[oak.budburst$species == 'rubra', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'rubra', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'rubra', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'rubra', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'rubra', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'rubra', 'individual_id'])))

bud.alba.list <- list(y = oak.budburst[oak.budburst$species == 'alba', 'MeanGDD5.cum'], n = length(oak.budburst[oak.budburst$species== 'alba', 'MeanGDD5.cum']),
                      loc = as.numeric(factor(oak.budburst[oak.budburst$species == 'alba', 'site.id'])), nLoc = length(unique(oak.budburst[oak.budburst$species == 'alba', 'site.id'])),
                      pln = as.numeric(factor(oak.budburst[oak.budburst$species == 'alba', 'individual_id'])), nPln = length(unique(oak.budburst[oak.budburst$species == 'alba', 'individual_id'])))



#running the model

green.mod   <- jags.model (file = textConnection(NPN_regression),
                           data = green.list,
                           inits = inits,
                           n.chains = 3)
midgreen.mod <- jags.model (file = textConnection(NPN_regression),
                            data = midgreen.list,
                            inits = inits,
                            n.chains = 3)
bud.mont.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.mont.list,
                            inits = inits,
                            n.chains = 3)
bud.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.alba.list,
                            inits = inits,
                            n.chains = 3)
bud.gamb.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.gamb.list,
                            inits = inits,
                            n.chains = 3)
bud.rubr.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.rubr.list,
                            inits = inits,
                            n.chains = 3)
bud.ilic.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.ilic.list,
                            inits = inits,
                            n.chains = 3)
bud.imbr.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.imbr.list,
                            inits = inits,
                            n.chains = 3)
bud.macr.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.macr.list,
                            inits = inits,
                            n.chains = 3)
bud.palu.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.palu.list,
                            inits = inits,
                            n.chains = 3)
bud.shum.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.shum.list,
                            inits = inits,
                            n.chains = 3)
bud.velu.mod <- jags.model (file = textConnection(NPN_regression),
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
bud.first.mean <- rbind(bud.stats.alba, bud.stats.mont, bud.stats.gamb, bud.stats.rubr, bud.stats.ilic, bud.stats.imbr
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


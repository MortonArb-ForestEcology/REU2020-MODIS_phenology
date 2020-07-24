#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHER YOU DO THINGS
path.MODIS <- "../data_processed/MODIS"
species.name <- "Q.alba"

dat.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_GDD5_", species.name, ".csv")))
dat.MODIS$species <- species.name
#st up for read is of both greenup and midgreenup

MODIS_regression <- "
  model{
    
    for(k in 1:n){
      mu[k] <- THRESH[sp[k]]  
      y[k] ~ dnorm(mu[k], sPrec)
    }
    
    for(j in 1:nSp){
      THRESH[j] <- Site[loc[j]] + a[j]
      a[j] ~ dnorm(0, aPrec)
    }

    for(t in 1:nLoc){
    Site[t] <-  c[t]
    c[t] ~ dnorm(0, cPrec[t])
    cPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    sPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
  }
  "

nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(sPrec = runif(1,1/200,1/20))
}

green.list <- list(y = dat.MODIS$GDD5.cum, n = length(dat.MODIS$GDD5.cum),
                      loc = as.numeric(factor(dat.MODIS$site)), nLoc = length(unique(dat.MODIS$site)),
                   sp=as.numeric(factor(dat.MODIS$species)) , nSp =length(unique(dat.MODIS$species)))

green.mod <- jags.model (file = textConnection(MODIS_regression),
                            data = green.list,
                            inits = inits,
                            n.chains = 3)


green.out <- coda.samples (model = green.mod,
                           variable.names = c("THRESH", "sPrec", "Site", "aPrec", "cPrec"),
                           n.iter = 10000)

summary(green.out)

gelman.diag(green.out)

burnin = 9000 
green.burn <- window(green.out, start= burnin)

green.stats <- as.data.frame(as.matrix(green.stats))
summary(green.stats)

# save the outputs
path.mod.firstmean <- "../data_processed/mod.firstmean.MortonArb"
if(!dir.exists(path.mod.firstmean)) dir.create(path.mod.firstmean)
write.csv(green.stats, file.path(path.mod.firstmean, "MODIS_THRESH_bud_firstmean_alba.csv"), row.names=F) 

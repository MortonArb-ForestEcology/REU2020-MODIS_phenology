#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHER YOU DO THINGS
path.NPN <- "../data_processed/NPN"
species.name <- "Q.alba"

#-----------------------------
alba.budburst <- read.csv(file.path(path.NPN, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')))
alba.leaves <- read.csv(file.path(path.NPN, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')))
head(alba.budburst)
summary(alba.budburst)


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
  inits[[i]] <- list(sPrec = runif(1,1/200,1/20))
}


#---------------------------------------------------------#
#This section actually runs the model and then provides ways to check the output and clean it
#---------------------------------------------------------#
#Converting to list format needed for JAGs

#bud burst lists for dat.budburst$MeanGDD5.cum
bud.alba.list <- list(y = alba.budburst$first.min, n = length(alba.budburst$first.min),
                      loc = as.numeric(factor(alba.budburst$site_id)), nLoc = length(unique(alba.budburst$site_id)),
                      pln = as.numeric(factor(alba.budburst$individual_id)), nPln = length(unique(alba.budburst$individual_id)))




bud.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.alba.list,
                            inits = inits,
                            n.chains = 3)

#Converting the output into a workable format
#DO THINGS HERE SOMETIMES


bud.alba.out <- coda.samples (model = bud.alba.mod,
                              variable.names = c("THRESH", "sPrec"),
                              n.iter = 10000)


#plot(bud.alba.out)
summary(bud.alba.out)




gelman.diag(bud.alba.out)


#--------------------

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 9000                                ## determine convergence from GBR output
bud.alba.burn <- window(bud.alba.out, start= burnin)


bud.stats.alba <- as.data.frame(as.matrix(bud.alba.burn))
summary(bud.stats.alba)

# save the outputs
path.mod.firstmean <- "../data_processed/mod.firstmean.MortonArb"
if(!dir.exists(path.mod.firstmean)) dir.create(path.mod.firstmean)
write.csv(bud.stats.alba, file.path(path.mod.firstmean, "THRESH_bud_firstmean_alba.csv"), row.names=F) 


#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHERE YOU DO THINGS
path.NPN <- "../data_processed/NPN"
species.name <- "Q.alba"

#-----------------------------
alba.budburst <- read.csv(file.path(path.NPN, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')))
alba.leaves <- read.csv(file.path(path.NPN, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')))

head(alba.budburst)
summary(alba.budburst)

head(alba.leaves)
summary(alba.leaves)


NPN_regression <- "
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
    Site[t] <-  c[t] + ind[pln[t]]
    c[t] ~ dnorm(0, cPrec[t])
    cPrec[t] ~ dgamma(0.1, 0.01)
    }
    
    for(i in 1:nPln){
        ind[i] <-  b[i]
        b[i] ~ dnorm(0, bPrec)
    }
    aPrec ~ dgamma(0.1, 0.1)
    bPrec ~ dgamma(1, 0.1)
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

#bud burst lists for dat.budburst$MinGDD5.cum
bud.alba.list <- list(y = alba.budburst$MinGDD5.cum, n = length(alba.budburst$MinGDD5.cum),
                      loc = as.numeric(factor(alba.budburst$site_id)), nLoc = length(unique(alba.budburst$site_id)),
                      pln = as.numeric(factor(alba.budburst$individual_id)), nPln = length(unique(alba.budburst$individual_id)),
                      sp = as.numeric(factor(alba.budburst$species)), nSp = length(unique(alba.budburst$species)))

#leaves lists for dat.leaves$MinGDD5.cum
leaf.alba.list <- list(y = alba.leaves$MinGDD5.cum, n = length(alba.leaves$MinGDD5.cum),
                       loc = as.numeric(factor(alba.leaves$site_id)), nLoc = length(unique(alba.leaves$site_id)),
                       pln = as.numeric(factor(alba.leaves$individual_id)), nPln = length(unique(alba.leaves$individual_id)),
                       sp = as.numeric(factor(alba.leaves$species)), nSp = length(unique(alba.leaves$species)))



bud.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.alba.list,
                            n.chains = 3)

leaf.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = leaf.alba.list,
                            n.chains = 3)

#Converting the output into a workable format
#DO THINGS HERE SOMETIMES


bud.alba.out <- coda.samples (model = bud.alba.mod,
                              variable.names = c("Site", "cPrec", "bPrec", "THRESH", "sPrec"),
                              n.iter = 100000)

leaf.alba.out <- coda.samples (model = leaf.alba.mod,
                              variable.names = c("Site", "cPrec", "bPrec", "THRESH", "sPrec"),
                              n.iter = 100000)

gelman.diag(bud.alba.out)
gelman.diag(leaf.alba.out)
#site convergence is not right yet

summary(bud.alba.out)
summary(leaf.alba.out)

#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 90000                                ## determine convergence from GBR output
bud.alba.burn <- window(bud.alba.out, start= burnin)
leaf.alba.burn <- window(leaf.alba.out, start= burnin)


bud.stats.alba <- as.data.frame(as.matrix(bud.alba.burn))
summary(bud.stats.alba$THRESH) #look for THRESH Mean

leaf.stats.alba <- as.data.frame(as.matrix(leaf.alba.burn))
summary(leaf.stats.alba$THRESH) #look for THRESH Mean

#-----------------------
#summary statistics https://docs.google.com/spreadsheets/d/1c3OIhbKru-WJF3vmgUEUpltis22eYuaebYFEEPxKEtI/edit#gid=0

#GDD5 Threshold 95 CI NPN Budburst

round(quantile(bud.stats.alba$THRESH , c(0.025, 0.975), na.rm = T), digits = 1)

#GDD5 Threshold95 CI NPN Leaves
round(quantile(leaf.stats.alba$THRESH , c(0.025, 0.975), na.rm = T), digits = 1)

#----------------------

# save the outputs
path.mod.firstmin <- "../data_processed/mod.firstmin.Q.alba"
if(!dir.exists(path.mod.firstmin)) dir.create(path.mod.firstmin)
write.csv(bud.stats.alba, file.path(path.mod.firstmin, "THRESH_bud_NPN_alba.csv"), row.names=F) 
write.csv(leaf.stats.alba, file.path(path.mod.firstmin, "THRESH_leaf_NPN_alba.csv"), row.names=F) 


#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)
library(ggplot2)

#THIS IS WHERE YOU DO THINGS
path.NPN <- "../data_processed/NPN"
species.name <- "Q.alba"

#-----------------------------
alba.budburst <- read.csv(file.path(path.NPN, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')))
alba.leaves <- read.csv(file.path(path.NPN, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')))
head(alba.budburst)
summary(alba.budburst$MinGDD5.cum)
alba.budburst <- alba.budburst[alba.budburst$MinGDD5.cum>0,]
alba.budburst[alba.budburst$site_id == 35869 ,]


#how to exactly describe this?
bud.ind <- aggregate(site_id~individual_id, data=alba.budburst,
          FUN=min)

leaves.ind <- aggregate(site_id~individual_id, data=alba.leaves,
                     FUN=min)

NPN_regression <- "
  model{
  
    for(k in 1:n){
        mu[k] <- ind[pln[k]]  
        y[k] ~ dnorm(mu[k], sPrec)
    }
      
    for(j in 1:nSp){
      THRESH[j] <-  a[j]
      a[j] ~ dnorm(0, aPrec)
    }

    for(t in 1:nLoc){
      Site[t] <-  THRESH[sp[t]] + b[t]
      b[t] ~ dnorm(0, bPrec[t])
      bPrec[t] ~ dgamma(0.1, 0.1)
    }
    
    for(i in 1:nPln){
        ind[i] <-  Site[loc[i]] + c[i]
        c[i] ~ dnorm(0, cPrec)
    }
    
    sPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
    cPrec ~ dgamma(0.1, 0.1)
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
                      #The line below contains the main change. Using the ind data frame to match individuals to their sites
                      loc = as.numeric(factor(bud.ind$site_id)), nLoc = length(unique(alba.budburst$site_id)),
                      pln = as.numeric(factor(alba.budburst$individual_id)), nPln = length(unique(alba.budburst$individual_id)),
                      sp = as.numeric(factor(alba.budburst$species)), nSp = length(unique(alba.budburst$species)))

#bud burst lists for dat.leaves$MinGDD5.cum
leaf.alba.list <- list(y = alba.leaves$MinGDD5.cum, n = length(alba.leaves$MinGDD5.cum),
                      #The line below contains the main change. Using the ind data frame to match individuals to their sites
                      loc = as.numeric(factor(leaves.ind$site_id)), nLoc = length(unique(alba.leaves$site_id)),
                      pln = as.numeric(factor(alba.leaves$individual_id)), nPln = length(unique(alba.leaves$individual_id)),
                      sp = as.numeric(factor(alba.leaves$species)), nSp = length(unique(alba.leaves$species)))



bud.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = bud.alba.list,
                            n.chains = 3)

leaf.alba.mod <- jags.model (file = textConnection(NPN_regression),
                            data = leaf.alba.list,
                            n.chains = 3)


bud.alba.out <- coda.samples (model = bud.alba.mod,
                              variable.names = c("THRESH", "aPrec"),
                              n.iter = 100000)

leaf.alba.out <- coda.samples (model = leaf.alba.mod,
                              variable.names = c("THRESH", "aPrec"),
                              n.iter = 100000)


gelman.diag(bud.alba.out)
gelman.diag(leaf.alba.out)


#Removing burnin before convergence occurred -- this is the model "warmup"
burnin = 90000                                ## determine convergence from GBR output
bud.alba.burn <- window(bud.alba.out, start= burnin)
leaf.alba.burn <- window(leaf.alba.out, start= burnin)

bud.stats.alba <- as.data.frame(as.matrix(bud.alba.burn))
summary(bud.stats.alba)

leaf.stats.alba <- as.data.frame(as.matrix(leaf.alba.burn))
summary(leaf.stats.alba)

#--------------------------
#visualization
bud.stats.alba$name <- 'Breaking Leaf Buds'
leaf.stats.alba$name <- 'Leaves'
bud.stats.alba$type <- 'NPN'
leaf.stats.alba$type <- 'NPN'

#this is wonky! do not use unless you are Andrew for right now
NPN.stats <- rbind(bud.stats.alba, leaf.stats.alba)

leaf.stats.alba$name <- as.factor(leaf.stats.alba$name)
leaf.stats.alba$type <- as.factor(leaf.stats.alba$type)

summary(NPN.stats)

bud.stats.alba$sd <- 1/sqrt(bud.stats.alba[,"aPrec"])

THRESH.ci <- apply(as.matrix(bud.stats.alba$THRESH),2,quantile,c(0.055,0.5,0.945))
sd.ci <- apply(as.matrix(bud.stats.alba$sd),2,quantile,c(0.055,0.5,0.945))

library(ggplot2)
path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('Thresh_NPN_GDD5', species.name, '.png')))
ggplot(data= leaf.stats.alba) +
  ggtitle('Thermal Time Thresholds of two NPN metrics at sites of data for Quercus alba from 2008-2019') +
  geom_density(mapping = aes(x= THRESH, fill = name, color = name), alpha=0.5) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
dev.off()

#--------------------------
#summary statistics https://docs.google.com/spreadsheets/d/1c3OIhbKru-WJF3vmgUEUpltis22eYuaebYFEEPxKEtI/edit#gid=0

#GDD5 Threshold 95 CI for NPN Breaking Leaf Buds Greenup
round(summary(bud.stats.alba$THRESH), digits = 1)
round(quantile(bud.stats.alba$THRESH, c(0.025, 0.975), na.rm = T), digits = 1)

#GDD5 Threshold95 CI for NPN Leaves
round(summary(leaf.stats.alba$THRESH), digits = 1)
round(quantile(leaf.stats.alba$THRESH, c(0.025, 0.975), na.rm = T), digits = 1)

# save the outputs
path.mod.firstmin <- "../data_processed/mod.firstmin.Q.alba"
if(!dir.exists(path.mod.firstmin)) dir.create(path.mod.firstmin)
write.csv(bud.stats.alba, file.path(path.mod.firstmin, "THRESH_bud_firstmin_alba.csv"), row.names=F) 
write.csv(leaf.stats.alba, file.path(path.mod.firstmin, "THRESH_bud_firstmin_alba.csv"), row.names=F) 


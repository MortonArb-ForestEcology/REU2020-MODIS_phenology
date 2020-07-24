#This is the standard species hierarchical model designed for NPN data including a level for location

library(rjags)
library(coda)

#THIS IS WHER YOU DO THINGS
path.MODIS <- "../data_processed/MODIS"
species.name <- "Q.alba"

green.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_GDD5_15_", species.name, ".csv")))
green.MODIS$species <- species.name
midgreen.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_GDD5_50_", species.name, ".csv")))
midgreen.MODIS$species <- species.name


MODIS_regression <- "
  model{
  
    for(k in 1:n){
        mu[k] <- Site[loc[k]]  
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

    sPrec ~ dgamma(0.1, 0.1)
    aPrec ~ dgamma(0.1, 0.1)
  }
"

nchain = 3
inits <- list()
for(i in 1:nchain){
  inits[[i]] <- list(sPrec = runif(1,1/200,1/20))
}

green.list <- list(y = green.MODIS$GDD5.cum, n = length(green.MODIS$GDD5.cum),
                      loc = as.numeric(factor(green.MODIS$site)), nLoc = length(unique(green.MODIS$site)),
                   sp=as.numeric(factor(green.MODIS$species)) , nSp =length(unique(green.MODIS$species)))

midgreen.list <- list(y = midgreen.MODIS$GDD5.cum, n = length(midgreen.MODIS$GDD5.cum),
                   loc = as.numeric(factor(midgreen.MODIS$site)), nLoc = length(unique(midgreen.MODIS$site)),
                   sp=as.numeric(factor(midgreen.MODIS$species)) , nSp =length(unique(midgreen.MODIS$species)))

green.mod <- jags.model (file = textConnection(MODIS_regression),
                            data = green.list,
                            inits = inits,
                            n.chains = 3)

midgreen.mod <- jags.model (file = textConnection(MODIS_regression),
                         data = midgreen.list,
                         inits = inits,
                         n.chains = 3)

green.out <- coda.samples (model = green.mod,
                           variable.names = c("THRESH", "sPrec", "Site", "aPrec", "cPrec"),
                           n.iter = 100000)

midgreen.out <- coda.samples (model = midgreen.mod,
                           variable.names = c("THRESH", "sPrec", "Site", "aPrec", "cPrec"),
                           n.iter = 100000)


gelman.diag(green.out)
gelman.diag(midgreen.out)

burnin = 90000 
green.burn <- window(green.out, start= burnin)
midgreen.burn <- window(midgreen.out, start= burnin)

green.stats <- as.data.frame(as.matrix(green.burn))
summary(green.stats)

midgreen.stats <- as.data.frame(as.matrix(midgreen.burn))
summary(midgreen.stats)

#--------------------------
#visualization
#MODIS.stats <- rbind(green.stats) #need to combine the green.stats and midgreen.stats objects to visualize them together
  
library(ggplot2)
path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
#png(width= 750, filename= file.path(path.figures, paste0('Thresh_MODIS_', species.name, '.png')))
ggplot(data= green.stats) +
  ggtitle('Thermal Time Thresholds at First Minimum leaf Onset of Quercus at The Morton Arboretum') +
  geom_density(mapping = aes(x= THRESH), alpha=0.5) +
  scale_color_manual(values= "darkblue") +
  scale_fill_manual(values= "darkblue")  +
  scale_x_continuous('Thermal Time Threshold Minimum (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (%)')
dev.off()

#--------------------------
#summary statistics https://docs.google.com/spreadsheets/d/1c3OIhbKru-WJF3vmgUEUpltis22eYuaebYFEEPxKEtI/edit#gid=0

#GDD5 Threshold 95 CI MODIS 15% Greenup
summary(green.stats$THRESH)
round(quantile(green.stats$THRESH, c(0.025, 0.975), na.rm = T), digits = 1)

#GDD5 Threshold95 CI MODIS 50% MidGreenup
summary(midgreen.stats$THRESH)
round(quantile(midgreen.stats$THRESH, c(0.025, 0.975), na.rm = T), digits = 1)

# save the outputs
path.mod.firstmin <- "../data_processed/mod.firstmin.Q.alba"
if(!dir.exists(path.mod.firstmin)) dir.create(path.mod.firstmin)
write.csv(green.stats, file.path(path.mod.firstmin, "MODIS_THRESH_15_alba.csv"), row.names=F) 
write.csv(midgreen.stats, file.path(path.mod.firstmin, "MODIS_THRESH_50_alba.csv"), row.names=F) 

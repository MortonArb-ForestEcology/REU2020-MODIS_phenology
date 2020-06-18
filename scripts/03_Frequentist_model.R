#----------------------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Living Collections Phenology Forecasting MODIS REU
# Purpose: To use arb weather data and phenology monitoring data to create a predicitve model of bud burst timing
#          This script serves as the Frequentist model which will largely be for testing/comparison
# Inputs: dat.tst dataframe that is created by the 02_Met_data_transform.R script
#         met.all dataframe that is created by the 02_Met_data_transform.R script
# Outputs: Currently, a hindcast of a species modeled day of budburst vs observed date of budburst
# Notes: All script relating to met data is stolen from Christy ROllinson's script "02_MortonArb_Climate_BLoomTimes-1.r"
#        The majority of the rest is currently a modification of that same script by Christy

#-----------------------------------------------------------------------------------------------------------------------------------#

#loading ggplot for visualization 
library(ggplot2)


# Read in output of previous script
dat.comb <- read.csv(file.path("../data_raw/MODIS/", paste0("Clean_Greenup_", site.id, ".csv")))
#---------------------------------------------------------------------#
#If only running the Bayesian model than you can stop here. These may become fully seperate scripts down the line
#---------------------------------------------------------------------#

# Testing whether GDD5 is a good predictor of day 
dat.gdd5.lm <- lm(greenup.yday ~ GDD5.cum, data=dat.comb)
summary(dat.gdd5.lm)
plot(greenup.yday ~ GDD5.cum, data=dat.comb)
# abline(dat.gdd5.lm, col="red")


#Box plot of dat.burst to see the distribution. Macrocarpa has an outlier but seems plausible 
ggplot(data=dat.comb) +
  geom_boxplot(aes(x=greenup.year, y=GDD5.cum,)) +
  scale_y_continuous(name="Accumulated Warming") +
  # scale_fill_manual(name="Species", values=c("deeppink2", "mediumpurple1")) +
  guides(fill=F) +
  theme(panel.background=element_rect(fill=NA, color="black"),
        legend.text = element_text(face="italic"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face="bold.italic", color="black", size=rel(1), angle=-15, hjust=0),
        axis.title.y=element_text(face="bold", color="black", size=rel(1.5)),
        axis.text.y=element_text(color="black", size=rel(1)))


#Creating a dataframe with the mean values of these metrics for every year
dat.yr <- aggregate(met.all[,c("TMAX", "TMIN", "TMEAN", "PRCP", "SNOW")],
                    by=met.all[,c("STATION", "YEAR")],
                    FUN=mean, na.rm=T)

#removing years too before reliable measurments
dat.yr <- dat.yr[dat.yr$YEAR>=1922,]

#Creating a matrix of the right size to be filled with the distribution of predictions for that year
mat.yr <- array(dim=c(nrow(dat.yr), 1000, length(species)))
dimnames(mat.yr)[[1]] <- dat.yr$YEAR
dimnames(mat.yr)[[3]] <- species

#---------------------------------------------------------#
#This section is for calculating mean and sd of our observed data to base our model distribtuion off of
#---------------------------------------------------------#
#loading in nlme and lme4 for their linear mixed effect equations
library(nlme); library(lme4)

#creating a mean and sd for gdd
dat.gdd5.mean <- mean(dat.comb[,"GDD5.cum"], na.rm=T); 
dat.gdd5.sd <- sd(dat.comb[,"GDD5.cum"], na.rm=T)

#Creating a hierarchial mean to compare
mac.cue <- lme(GDD5.cum ~ 1, random=list(PlantNumber=~1), data=dat.comb, na.action=na.omit)
mac.summ <- summary(mac.cue)
MuMIn::r.squaredGLMM(mac.cue)
mod.cue.est <- mac.summ$tTable[,"Value"] # Hierarchical mean
mod.cue.se <- mac.summ$tTable[,"Std.Error"]
mod.cue.sd <- mac.summ$tTable[,"Std.Error"]*sqrt(mac.summ$tTable[,"DF"]+1)

# Compare to non-hierarchical
mod.cue.est; mod.cue.sd
dat.gdd5.mean; dat.gdd5.sd 

#Creating the distribtuion of gdd5 values to be ran through the calc.bud function
dat.gdd5.vec <- rnorm(1000, dat.gdd5.mean, dat.gdd5.sd)

#Function used to calculate bud burst day using gdd5
calc.bud <- function(x){min(dat.tmp[which(dat.tmp$GDD5.cum >= x),"YDAY"])}

#Filling a matrix with yday values calculating by running calc.bud on a distribution of gdd5.cum values at bud burst (dat.gdd5.vec)
i <- 1
for(i in 1:nrow(dat.yr)){
  YR=dat.yr$YEAR[i]
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  # summary(dat.tmp)
  if(nrow(dat.tmp)==0) next
  
  # Bloom time -- simple
  bud.pred <- calc.bud(dat.gdd5.mean)
  if(bud.pred != Inf) dat.yr[i,"bud.oak"] <- bud.pred
  bud.vec <- unlist(lapply(dat.gdd5.vec, calc.bud))
  bud.vec[bud.vec==Inf] <- NA
  
  mat.yr[i,] <- bud.vec
}


#---------------------------------------------------------#
#This section is for summarizing the model data and observed data so they can be visualized and compared
#---------------------------------------------------------#
#Calculating summary statistics of the matrix of values from each year
dat.yr$bud.mean <- apply(mat.yr, 1, mean, na.rm=T)
dat.yr$bud.sd   <- apply(mat.yr, 1, sd, na.rm=T)
dat.yr$bud.lb   <- apply(mat.yr, 1, quantile, 0.025, na.rm=T)
dat.yr$bud.ub   <- apply(mat.yr, 1, quantile, 0.975, na.rm=T)
summary(dat.yr) 


#Aggregating all indivudla measurements into one mean for the every year
oak.bud <- aggregate(dat.comb[,c("greenup.yday", "GDD5.cum")],
                     by=list(dat.comb$greenup.year),
                     FUN=mean, na.rm=F)


#Calculating sd values to put on our visualizations
oak.bud[,c("greenup.yday.sd", "GDD5.cum.sd")]  <- aggregate(dat.comb[,c("greenup.yday", "GDD5.cum")],
                                                    by=list(dat.comb$greenup.year),
                                                    FUN=sd, na.rm=F)[,c("greenup.yday", "GDD5.cum")]


#Graphing the output of the model vs. the observed data
ggplot(data=dat.yr[,]) +
  geom_ribbon(data=dat.yr[,], aes(x=YEAR, ymin=bud.lb, ymax=bud.ub, fill="Modeled"), alpha=0.5) +
  geom_point(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) +
  geom_line(data=dat.yr[,], aes(x=YEAR, y=bud.mean, color="Modeled"), alpha=0.8) + 
  geom_pointrange(data=oak.bud, aes(x=Group.1, y=greenup.yday, ymin=greenup.yday-greenup.yday.sd, ymax=greenup.yday+greenup.yday.sd,color="Observed"))+
  ggtitle("Modeled and Observed day of year of budburst for Quercus Macrocarpa")+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_text(size=rel(2.5), color="black"),
        axis.title=element_text(size=rel(2.5), face="bold"),)


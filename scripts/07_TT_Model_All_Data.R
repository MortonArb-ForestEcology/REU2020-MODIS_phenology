#combining the NPN and MODIS Threshold model outputs for Quercus alba
#data from 06_MODIS and 06_NPN scripts
#summary stattistics https://docs.google.com/spreadsheets/d/1c3OIhbKru-WJF3vmgUEUpltis22eYuaebYFEEPxKEtI/edit#gid=0
#--------------------------
species.name <- 'Q.alba'

#read in the rbinded data from the models
path.mod.firstmin <- "../data_processed/mod.firstmin.Q.alba"
if(!dir.exists(path.mod.firstmin)) dir.create(path.mod.firstmin)
NPN.stats <- read.csv(file.path(path.mod.firstmin, "THRESH_NPN_firstmin_alba.csv"))
MODIS.stats <- read.csv(file.path(path.mod.firstmin, 'THRESH_MODIS_alba.csv'))

summary(NPN.stats)
summary(MODIS.stats)

#--------------------------
#visualization with all thresholds
Q.alba.stats <- rbind(NPN.stats, MODIS.stats)

unique(Q.alba.stats$metric)
  
library(ggplot2)
path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)
png(width= 750, filename= file.path(path.figures, paste0('THRESHOLDS_GDD5_', species.name, '.png')))
ggplot(data= Q.alba.stats) +
  ggtitle('Thermal Time Thresholds for NPN and MODIS metrics at sites of data for Quercus alba') +
  geom_density(mapping = aes(x= THRESH, fill = metric, color = metric), alpha=0.5) +
  scale_color_manual(values=c("coral2", "coral3", "darkolivegreen", "darkgreen")) +
  scale_fill_manual(values=c("coral2", "coral3", "darkolivegreen", "darkgreen"))  +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (Probability)')
dev.off()

# save the outputs
path.mod <- "../data_processed/Q.alba.model"
if(!dir.exists(path.mod)) dir.create(path.mod)
write.csv(Q.alba.stats, file.path(path.mod, paste0('THRESHOLDS_GDD5_', species.name, '.csv')), row.names=F) 


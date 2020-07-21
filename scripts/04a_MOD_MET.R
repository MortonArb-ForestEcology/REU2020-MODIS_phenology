#Here the raw data from the DAYMET and MODIS will be combined
#This will give the MODIS data a GDD5 and GDD5.cum for the site given
library(MODISTools)
library(ggplot2)
library(daymetr)

species.name <- 'Q.alba'

path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)

leaf.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_Greenup_Quercus_", species.name, ".csv")))

summary(leaf.MODIS)

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
df.leaf <- read.csv(file.path(path.DAYMET, paste0("DAYMET_Data_Processed_", species.name, ".csv")))
summary(df.leaf)

leaf.MODIS$GDD5.cum <- NA

hist(leaf.MODIS$greenup.yday)

#problem loop, possibly a problem in all the NAs in df.leaf$site
leaf.MODIS$site <- as.character(leaf.MODIS$site)

for(i in 1:nrow(leaf.MODIS)){
  
  yr.now <- leaf.MODIS[i, "greenup.year"]
  yday.now <- leaf.MODIS[i, "greenup.yday"]
  site.now <- leaf.MODIS[i, 'site']
  
  if(is.na(leaf.MODIS$greenup.yday[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  leaf.MODIS[i,"GDD5.cum"] <-df.leaf[df.leaf$year==yr.now & df.leaf$yday==yday.now & df.leaf$site==site.now,"GDD5.cum"]
}
summary(leaf.MODIS)
unique(leaf.MODIS$BAND)


path.figures <- '../REU2020-MODIS_phenology/figures'
if(!dir.exists(path.figures)) dir.create(path.figures, recursive=T)
png(filename= file.path(path.figures, paste0('MODIS_Met_Plot_', species.name, '.png')))
ggplot(data = leaf.MODIS, mapping = aes(x= GDD5.cum, y = BAND)) +
  ggtitle('Thermal Time at 15% Greenup at NPN Quecus alba from 2001-2019') +
  geom_line()
dev.off()

dat.processed <- file.path("../data_processed/MODIS")
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(leaf.MODIS, file.path(dat.processed, paste0("MODIS_Quercus_alba_GDD5_", species.name, ".csv")), row.names=F)


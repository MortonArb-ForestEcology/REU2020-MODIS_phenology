#Here the raw data from the DAYMET and MODIS will be combined
#This will give the MODIS data a GDD5 and GDD5.cum for the site given
library(MODISTools)
library(ggplot2)
library(daymetr)

site.id <- 'MortonArb'

path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)
dat.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_Greenup_Raw_", site.id, ".csv")))
summary(dat.MODIS)

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
df.met <- read.csv(file.path(path.DAYMET, paste0("DAYMET_Data_Raw_", site.id, ".csv")))
summary(df.met)

for(i in 1:nrow(dat.MODIS)){
  # dat.MODIS[i,]
  # We need to use greenup.year, greenup.yday
  yr.now <- dat.MODIS[i, "greenup.year"] # same as dat.MODIS[i, "greenup.yr"]
  yday.now <- dat.MODIS[i, "greenup.yday"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.MODIS[i,"GDD5.cum"] <- df.met[df.met$year==yr.now & df.met$yday==yday.now,"GDD5.cum"]
}

head(dat.MODIS)
unique(dat.MODIS$BAND)

path.figures <- '../REU2020-MODIS_phenology/figures'
if(!dir.exists(path.figures)) dir.create(path.figures, recursive=T)
png(filename= file.path(path.figures, paste0('MODIS_Met_Plot_', site.id, '.png')))
ggplot(data = dat.MODIS, mapping = aes(x = BAND, y = GDD5.cum)) +
  ggtitle('Thermal Time at 15% Greenup and 50% MidGreenuup at The Morton Arboretum from 2001-2019') +
  geom_boxplot()
dev.off()

dat.processed <- file.path("../data_processed/MODIS")
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.MODIS, file.path(dat.processed, paste0("MODIS_MET_GDD5", site.id, ".csv")), row.names=F)

#Here the products from the daymet and MODIS scripts will be combined

#packages
#library(MODISTools)
#library(ggplot2)
#library(daymetr)

#could be changed to be individual species not site
path.MODIS <- '../data_raw/MODIS'
site.id <- 'MortonArb'

dat.MODIS <- read.csv(file.path('../data_raw/MODIS', paste0("MODIS_Greenup_", site.id, ".csv")))
summary(dat.MODIS)

df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("TEST_DAYMET_", site.id, ".csv")))
summary(df.met)

# What we need to do: get GDD extracted from df.met for dates in dat.MODIS
# Dates are defined by unique year and yday combinations
# In dat.MODIS each row is a unique observation; with a mostly unique date --> need a GDD.cum for each row

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

path.png <- '../figures/MODIS_Met'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)

png(filename= file.path(path.png, paste0('MODIS_Met_Plot_', site.id, '.png')))
ggplot(data = dat.MODIS, mapping = aes(x = BAND, y = GDD5.cum)) +
  #facet_wrap(~BAND, scales="free") +
  geom_boxplot()
dev.off()

summary(dat.MODIS)

dat.out <- file.path("../data_processed")
if(!dir.exists(dat.out)) dir.create(dat.out)
write.csv(dat.MODIS, file.path(dat.out, paste0("MODIS_MET_", site.id, ".csv")), row.names=F)

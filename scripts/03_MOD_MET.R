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

Mod.Met <- merge(dat.MODIS, df.met)
summary(Mod.Met)

#This will clean out any variables that are unnecessary
#Mod.Met <- Mod.Met[-c(4:5, 14, 19)]

#check to see what is left
names(Mod.Met)

write.csv(Mod.Met, file.path(path.MODIS, paste0("MOD_MET_", site.id, ".csv")), row.names=FALSE)

#Here the raw data from the DAYMET and MODIS will be combined
#This will give the MODIS data a GDD5 and GDD5.cum for the site given
library(MODISTools)
library(daymetr)

species.name <- 'Q.alba'

#bring in the MODIS data for 15%Greenup which has a filter of DOY 182 on it.
path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)

leaf.MODIS <- read.csv(file.path(path.MODIS, paste0("MODIS_Greenup_Quercus_", species.name, ".csv")))

summary(leaf.MODIS)

#bring in the DAYMET data which was collected using the NPN sites as a proxy.
path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
df.leaf <- read.csv(file.path(path.DAYMET, paste0("DAYMET_Data_Processed_", species.name, ".csv")))
summary(df.leaf)

#fill a new column for GDD5
leaf.MODIS$GDD5.cum <- NA

hist(leaf.MODIS$greenup.yday)

#Making sure site is read as a character string and not a factor
leaf.MODIS$site <- as.character(leaf.MODIS$site)

#pasing the MODIS data through the DAYMET data and extracting the DAYMET GDD5 where MODIS matches in 3 places (Year, DOY, Site)
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

#Getting npn site names
#THIS WILL GIVE YOU AN ERROR BUT IT WORKS
#!!!!! IGNORE THE ERROR !!!!!!

site_names <- rnpn::npn_stations()

#Giving the sites their name
leaf.MODIS$site_name <- site_names$station_name[match(leaf.MODIS$site, site_names$station_id)]

#Makigns ure different locations with the same name are given unique names by adding site_id
for(Name in unique(leaf.MODIS$site_name)){
  dat.tmp <- leaf.MODIS[leaf.MODIS$site_name == Name,]
  if(length(unique(dat.tmp$site)) >1){
    dat.tmp$site_name <- paste(dat.tmp$site_name, dat.tmp$site, sep="_")
  }
  leaf.MODIS[leaf.MODIS$site_name==Name, "site_name"] <- dat.tmp$site_name
}

#checking which MODIS data is present
unique(leaf.MODIS$BAND)

#visual for MODIS 15%Greenup thermal time accumulation
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('MODIS_Greenup_GDD5_', species.name, '.png')))

hist(leaf.MODIS$GDD5.cum)

dev.off()

#Saving the outputted MODIS data which now has a GDD5.cum and actual site names
dat.processed <- file.path("../data_processed/MODIS")
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(leaf.MODIS, file.path(dat.processed, paste0("MODIS_GDD5_", species.name, ".csv")), row.names=F)


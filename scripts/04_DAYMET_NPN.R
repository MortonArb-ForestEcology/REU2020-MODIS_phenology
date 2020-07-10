

#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_", site.id, ".csv")))
summary(df.met)

#get cleaned NPN data
path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
dat.budburst <- read.csv(file.path(path.clean, paste0('Quercus_bud_', site.id, '.csv')))
dat.leaves <- read.csv(file.path(path.clean, paste0('Quercus_leaf_', site.id, '.csv')))
#----------------------------

head(dat.budburst)
head(dat.leaves)

unique(dat.budburst$individual_id)
unique(dat.leaves$individual_id)

#these two yday values are calculated to different decimals. One has to conform to the other.
dat.budburst$first.mean <- round(dat.budburst$first.mean, digits = 0)
dat.leaves$first.mean <- round(dat.leaves$first.mean, digits = 0)

#-------------------------------
#Bud Burst Onset GDD5
for(i in 1:nrow(dat.budburst)){
 
  bud.year <- dat.budburst[i, "year"]
  bud.yday <- dat.budburst[i, "first.mean"]

  if(is.na(dat.budburst$first.mean[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.budburst[i,"GDD5.cum"] <-df.met[df.met$year==bud.year & df.met$yday==bud.yday,"GDD5.cum"]
}

tail(dat.budburst)

#------------------------------
#Leaves Onset GDD5
for(i in 1:nrow(dat.leaves)){
  
  bud.year <- dat.leaves[i, "year"]
  bud.yday <- dat.leaves[i, "first.mean"]
  
  if(is.na(dat.leaves$first.mean[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.leaves[i,"GDD5.cum"] <-df.met[df.met$year==bud.year & df.met$yday==bud.yday,"GDD5.cum"]
}

tail(dat.leaves)

library(ggplot2)

path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)

png(filename= file.path(path.png, paste0('Budburst_Onset_', site.id, '_NPN.png')))

ggplot(data = dat.budburst, mapping = aes(x= GDD5.cum, y= first.mean)) + 
  facet_wrap(dat.budburst$year) +
  geom_point(mapping=aes(color=species)) +
  ggtitle('Growing Degree Days for 12 Quercus species taken from NPN in 2017-2019 at The Morton Arboretum')+
  scale_x_continuous('5C Growing Degree Days')+
  scale_y_continuous('Bud Burst Onset (DOY)')
dev.off()

png(filename= file.path(path.png, paste0('Leaves_Onset_', site.id, '_NPN.png')))

ggplot(data = dat.leaves, mapping = aes(x= GDD5.cum, y= first.mean)) + 
  facet_wrap(dat.leaves$year) +
  geom_point(mapping=aes(color=species)) +
  ggtitle('Growing Degree Days for 12 Quercus species taken from NPN in 2017-2019 at The Morton Arboretum')+
  scale_x_continuous('5C Growing Degree Days')+
  scale_y_continuous('Leaves Onset (DOY)')
dev.off()
dat.processed <- '../data_processed/'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud', site.id, '_NPN_MET.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf', site.id, '_NPN_MET.csv')), row.names=F)

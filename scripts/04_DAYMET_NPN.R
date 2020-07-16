#get DAYMET data


site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_Raw_", site.id, ".csv")))
summary(df.met)

#get cleaned NPN data
path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
dat.budburst <- read.csv(file.path(path.clean, paste0('NPN_Quercus_bud_', site.id, '.csv')))
dat.leaves <- read.csv(file.path(path.clean, paste0('NPN_Quercus_leaf_', site.id, '.csv')))

#----------------------------
head(dat.budburst)
head(dat.leaves)

#these two yday values are calculated to different decimals. One has to conform to the other.
dat.budburst$first.mean <- round(dat.budburst$first.mean, digits = 0)
dat.budburst$first.min <- round(dat.budburst$first.min, digits = 0)
dat.leaves$first.mean <- round(dat.leaves$first.mean, digits = 0)
dat.leaves$first.min <- round(dat.leaves$first.min, digits = 0)

#-------------------------------
#Bud Burst Mean Onset GDD5

for(i in 1:nrow(dat.budburst)){
 
  Meanbud.year <- dat.budburst[i, "year"]
  Meanbud.yday <- dat.budburst[i, "first.mean"]

  if(is.na(dat.budburst$first.mean[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.budburst[i,"MeanGDD5.cum"] <-df.met[df.met$year==Meanbud.year & df.met$yday==Meanbud.yday,"GDD5.cum"]
}

#-------------------------------
#Bud Burst Minimum Onset GDD5

for(i in 1:nrow(dat.budburst)){
  
  Minbud.year <- dat.budburst[i, "year"]
  Minbud.yday <- dat.budburst[i, "first.min"]
  
  if(is.na(dat.budburst$first.min[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.budburst[i,"MinGDD5.cum"] <-df.met[df.met$year==Minbud.year & df.met$yday==Minbud.yday,"GDD5.cum"]
}

#checking for new GDD5 columns
summary(dat.budburst)

#------------------------------
#Leaves Mean Onset GDD5

for(i in 1:nrow(dat.leaves)){
  
  Meanleaf.year <- dat.leaves[i, "year"]
  Meanleaf.yday <- dat.leaves[i, "first.mean"]
  
  if(is.na(dat.leaves$first.mean[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.leaves[i,"MeanGDD5.cum"] <-df.met[df.met$year==Meanleaf.year & df.met$yday==Meanleaf.yday,"GDD5.cum"]
}

#------------------------------
#Leaves Minimum Onset GDD5

for(i in 1:nrow(dat.leaves)){
  
  Minleaf.year <- dat.leaves[i, "year"]
  Minleaf.yday <- dat.leaves[i, "first.min"]
  Minleaf.site <- dat.leaves[i, 'site_id']
  if(is.na(dat.leaves$first.min[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.leaves[i,"MinGDD5.cum"] <-df.met[df.met$year==Minleaf.year & df.met$yday==Minleaf.yday & df.met$site==Minleaf.site,"GDD5.cum"]
}

#checking for new GDD5 columns
summary(dat.leaves)

#------------------------------

library(ggplot2)
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)

#visual for bud burst first.mean thermal time
png(filename= file.path(path.png, paste0('Budburst_firstmean_', site.id, '_NPN.png')))

ggplot(data = dat.budburst, mapping = aes(x = species, y = MeanGDD5.cum)) +
  ggtitle('2017-2019 First Mean Thermal Time at Bud Burst Onset of 12 Quercus at The Morton Arboretum') +
  geom_boxplot() +
  scale_y_continuous('Mean Bud Burst Onset (MeanGDD5.cum)') +
  scale_x_discrete('Quercus')
dev.off()

#visual for bud burst first.min thermal time
png(filename= file.path(path.png, paste0('Budburst_firstmin_', site.id, '_NPN.png')))

ggplot(data = dat.budburst, mapping = aes(x = species, y = MinGDD5.cum)) +
  ggtitle('2017-2019 First Minimum Thermal Time at Bud Burst Onset of 12 Quercus at The Morton Arboretum') +
  geom_boxplot() +
  scale_y_continuous('Minimum Bud Burst Onset (MinGDD5.cum)') +
  scale_x_discrete('Quercus')
dev.off()

#visual for Leaves first.mean thermal time
png(filename= file.path(path.png, paste0('Leaves_firstmean_', site.id, '_NPN.png')))

ggplot(data = dat.leaves, mapping = aes(x = species, y = MeanGDD5.cum)) +
  ggtitle('2017-2019 First Mean Thermal Time at Leaf Onset of 12 Quercus at The Morton Arboretum') +
  geom_boxplot() +
  scale_y_continuous('Mean Leaf Onset (MeanGDD5.cum)') +
  scale_x_discrete('Quercus')
dev.off()

#visual for Leaves first.min thermal time
png(filename= file.path(path.png, paste0('Leaves_firstmin_', site.id, '_NPN.png')))

ggplot(data = dat.leaves, mapping = aes(x = species, y = MinGDD5.cum)) +
  ggtitle('2017-2019 First Minimum Thermal Time at Leaf Onset of 12 Quercus at The Morton Arboretum') +
  geom_boxplot() +
  scale_y_continuous('Minimum Leaf Onset (MinGDD5.cum)') +
  scale_x_discrete('Quercus')
dev.off()

#----------------------------

dat.processed <- '../data_processed/NPN'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud_GDD5_', site.id, '_NPN.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf_GDD5_', site.id, '_NPN.csv')), row.names=F)




#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_", site.id, ".csv")))
summary(df.met)

#get NPN data
path.npn <- "../data_raw/NPN/"
if(!dir.exists(path.npn)) dir.create(path.npn)
oak.leaf <- read.csv(file.path(path.npn, paste0('Quercus_', site.id, '.csv')))
#----------------------------

head(oak.leaf)

oak.leaf$phases.yday <- (oak.leaf$first_yes_doy + oak.leaf$last_yes_doy)/2 #get a mean of each phenophase yday

#fixes the loop by getting rid of the .00 decimal of oak.leaf$phases.yday to match df.met$yday
oak.leaf$phases.yday <- round(oak.leaf$phases.yday)

unique(oak.leaf$individual_id)

for(i in 1:nrow(oak.leaf)){
  # We need to use med.bud
  bud.yday <- oak.leaf[i, 'phases.yday']
  bud.year <- oak.leaf[i, "first_yes_year"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  oak.leaf[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.yday, "GDD5.cum"]
}

tail(oak.leaf)

#this specifies the phenophase by its id. 371 is 'Breaking Leaf Buds'
oak.budburst <- oak.leaf[oak.leaf$phenophase_id == '371',]
summary(oak.budburst)

oak.leaves <- oak.leaf[oak.leaf$phenophase_id == '483',]
summary(oak.leaves)

library(ggplot2)

path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('Quercus_Plot_', site.id, '_NPN.png')))

ggplot(data = oak.budburst, mapping = aes(x= GDD5.cum, y= phases.yday)) + 
  facet_wrap(oak.budburst$first_yes_year) +
  geom_point()

dev.off()
dat.processed <- '../data_processed/'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(oak.budburst, file.path(dat.processed, paste0('Quercus_', site.id, '_NPN_MET.csv')), row.names=F)
write.csv(oak.leaves, file.path(dat.processed, paste0('Quercus_Leaves', site.id, '_NPN_MET.csv')), row.names=F)

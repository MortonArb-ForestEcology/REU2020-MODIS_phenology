

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

budburst.list <- list(y = dat.budburst, n = length(dat.budburst$first.mean))

dat.budburst$GDD5.cum <- NA
#Need a way to pass over the NA values in dat.budburst$first.mean
for(i in 1:nrow(dat.budburst)){
 
  bud.year <- dat.budburst[i, "year"]
  bud.yday <- dat.budburst[i, "first.mean"]

  if(dat.budburst[dat.budburst$first.mean == is.na,]) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.budburst[i,"GDD5.cum"] <-df.met[df.met$year==bud.year & df.met$yday==bud.yday,"GDD5.cum"]
}

summary(dat.budburst)

tail(dat.budburst)

library(ggplot2)

path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('Quercus_Plot_', site.id, '_NPN.png')))

ggplot(data = dat.budburst, mapping = aes(x= GDD5.cum, y= first.mean)) + 
  facet_wrap(dat.budburst$year) +
  geom_point()

dev.off()
dat.processed <- '../data_processed/'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud', site.id, '_NPN_MET.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf', site.id, '_NPN_MET.csv')), row.names=F)

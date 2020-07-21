#get DAYMET data

#Getting npn site names
#THIS WILL GIVE YOU AN ERROR BUT IT WORKS
#IGNORE THE ERROR
site_names <- rnpn::npn_stations()


species.name <- 'Q.alba'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_Processed_", species.name, ".csv")))
summary(df.met)

#get cleaned NPN data
path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
dat.leaves <- read.csv(file.path(path.clean, paste0('NPN_Quercus_leaf_', species.name, '.csv')))
dat.budburst <- read.csv(file.path(path.clean, paste0('NPN_Quercus_bud_', species.name, '.csv')))

#----------------------------
head(dat.budburst)
head(dat.leaves)

#these two yday values are calculated to different decimals. One has to conform to the other.
dat.budburst$first.min <- round(dat.budburst$first.min, digits = 0)
dat.leaves$first.min <- round(dat.leaves$first.min, digits = 0)

#-------------------------------
#Bud Burst Minimum Onset GDD5

for(i in 1:nrow(dat.budburst)){
  
  Minbud.year <- dat.budburst[i, "year"]
  Minbud.yday <- dat.budburst[i, "first.min"]
  Minbud.site <- dat.budburst[i, 'site_id']
  
  if(is.na(dat.budburst$first.min[i])) next
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.MODIS row we're working with
  dat.budburst[i,"MinGDD5.cum"] <-df.met[df.met$year==Minbud.year & df.met$yday==Minbud.yday & df.met$site==Minbud.site,"GDD5.cum"]
}

#checking for new GDD5 columns
summary(dat.budburst)

#Giving the sites their name
dat.budburst$site_name <- site_names$station_name[match(dat.budburst$site_id, site_names$station_id)]

#Makigns ure different locations with the same name are given unique names by adding site_id
for(Name in unique(dat.budburst$site_name)){
  dat.tmp <- dat.budburst[dat.budburst$site_name == Name,]
  if(length(unique(dat.tmp$site_id)) >1){
    dat.tmp$site_name <- paste(dat.tmp$site_name, dat.tmp$site_id, sep="_")
  }
  dat.budburst[dat.budburst$site_name==Name, "site_name"] <- dat.tmp$site_name
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

#Giving the sites their name
dat.leaves$site_name <- site_names$station_name[match(dat.leaves$site_id, site_names$station_id)]

#Makigns ure different locations with the same name are given unique names by adding site_id
for(Name in unique(dat.leaves$site_name)){
  dat.tmp <- dat.leaves[dat.leaves$site_name == Name,]
  if(length(unique(dat.tmp$site_id)) >1){
    dat.tmp$site_name <- paste(dat.tmp$site_name, dat.tmp$site_id, sep="_")
  }
  dat.leaves[dat.leaves$site_name==Name, "site_name"] <- dat.tmp$site_name
}


#------------------------------

library(ggplot2)
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)

#visual for bud burst first.min thermal time
png(filename= file.path(path.png, paste0('Budburst_firstmin_', species.name, '_NPN.png')))

hist(dat.budburst$MinGDD5.cum)

dev.off()

map.us <- map_data("state")
png() #file path in here in here
ggplot() +
  coord_fixed(1.3) +
  ggtitle("test") +#Pick title here
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.budburst, aes(x=longitude, y=latitude), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#visual for Leaves first.min thermal time
png(filename= file.path(path.png, paste0('Leaves_firstmin_', species.name, '_NPN.png')))

hist(dat.leaves$MinGDD5.cum)

dev.off()

map.us <- map_data("state")
png() #file path in here in here
ggplot() +
  coord_fixed(1.3) +
  ggtitle("test") +#Pick title here
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.leaves, aes(x=longitude, y=latitude), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#----------------------------

dat.processed <- '../data_processed/NPN'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')), row.names=F)


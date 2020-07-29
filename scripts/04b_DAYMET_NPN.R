#get DAYMET data

#Getting npn site names
#THIS WILL GIVE YOU AN ERROR BUT IT WORKS
#IGNORE THE ERROR
site_names <- rnpn::npn_stations()

#bring in the DAYMET data that GDD5 as been calculated for 2000-2019
species.name <- 'Q.alba'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_Processed_", species.name, ".csv")))
summary(df.met)

#bring in cleaned NPN data
path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
dat.leaves <- read.csv(file.path(path.clean, paste0('NPN_Quercus_leaf_', species.name, '.csv')))
dat.budburst <- read.csv(file.path(path.clean, paste0('NPN_Quercus_bud_', species.name, '.csv')))

#----------------------------

#the yday values from NPN and DAYMET are rounded to different decimals. One has to conform to the other.
dat.budburst$first.min <- round(dat.budburst$first.min, digits = 0)
dat.leaves$first.min <- round(dat.leaves$first.min, digits = 0)

#-------------------------------
#Bud Burst Minimum Onset GDD5, outputting the GDD5.cum of where NPN matches DAYMET in year, DOY, and site

for(i in 1:nrow(dat.budburst)){
  
  Minbud.year <- dat.budburst[i, "year"]
  Minbud.yday <- dat.budburst[i, "first.min"]
  Minbud.site <- dat.budburst[i, 'site_id']
  
  if(is.na(dat.budburst$first.min[i])) next
  # We need to get certain rows --> we need 3 pieces of info to match
  
  dat.budburst[i,"MinGDD5.cum"] <-df.met[df.met$year==Minbud.year & df.met$yday==Minbud.yday & df.met$site==Minbud.site,"GDD5.cum"]
}

#checking for new GDD5 columns
summary(dat.budburst)

#Giving the sites their name
dat.budburst$site_name <- site_names$station_name[match(dat.budburst$site_id, site_names$station_id)]

#Making sure different locations with the same name are given unique names by adding site_id
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
  
  dat.leaves[i,"MinGDD5.cum"] <-df.met[df.met$year==Minleaf.year & df.met$yday==Minleaf.yday & df.met$site==Minleaf.site,"GDD5.cum"]
}

#checking for new GDD5 columns
summary(dat.leaves)

#Giving the sites their name
dat.leaves$site_name <- site_names$station_name[match(dat.leaves$site_id, site_names$station_id)]

#Making sure different locations with the same name are given unique names by adding site_id
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

#visual for bud burst first.min thermal time accumulation
png(filename= file.path(path.png, paste0('Budburst_firstmin_', species.name, '_NPN.png')))

hist(dat.budburst$MinGDD5.cum)

dev.off()

#getting a map of all the NPN sites which collected observation for 'Breaking Leaf Buds' from 2000 to 2019
#install.packages('maps')
map.us <- map_data("state")
png(filename= file.path(path.png, paste0('NPN_sitemap_budburst', species.name, '.png')))
ggplot() +
  coord_fixed(1.3) +
  ggtitle("Map of NPN sites where 'Breaking Leaf Buds' for Q. alba was observed from 2000-2019") +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.budburst, aes(x=longitude, y=latitude, color = 'red'), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#visual for Leaves first.min thermal time accumulation
png(filename= file.path(path.png, paste0('Leaves_firstmin_', species.name, '_NPN.png')))
hist(dat.leaves$MinGDD5.cum)
dev.off()

#----------------------------------
#summary statistics

#GDD5 95 CI NPN Budburst
round(quantile(dat.budburst$MinGDD5.cum, c(0.025, 0.975), na.rm = T), digits = 1)

#GDD5 95 CI NPN Leaves
round(quantile(dat.leaves$MinGDD5.cum, c(0.025, 0.975), na.rm = T), digits = 1)

#YDAY 95 CI NPN Budburst
round(quantile(dat.budburst$first.min , c(0.025, 0.975), na.rm = T), digits = 0)

#YDAY 95 CI NPN Leaves
round(quantile(dat.leaves$first.min , c(0.025, 0.975), na.rm = T), digits = 0)

#----------------------------------

#number of observations for NPN Budburst
summary(dat.budburst)
dim(dat.budburst[!is.na(dat.budburst$site_id),])
length(unique(dat.budburst$site_id))

#number of observations for NPN Leaves
summary(dat.leaves)
dim(dat.leaves[!is.na(dat.leaves$site_id),])
length(unique(dat.leaves$site_id))

#----------------------------------

#getting a map of all the NPN sites which collected observation for 'Leaves' from 2000 to 2019
map.us <- map_data("state")
png(filename= file.path(path.png, paste0('NPN_sitemap_leaves', species.name, '.png')))
ggplot() +
  coord_fixed(1.3) +
  ggtitle("Map of NPN sites where 'Leaves' for Q. alba was observed from 2000-2019") +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=dat.leaves, aes(x=longitude, y=latitude, color = 'red'), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#----------------------------
#conceptual figures

summary(dat.budburst)#see what data there is to work with 
summary(dat.leaves)

#read in the MODIS data from the 04a_MOD_MET script
path.modis <- file.path("../data_processed/MODIS")
if(!dir.exists(path.modis)) dir.create(path.modis)
dat.green <- read.csv(file.path(path.modis, paste0("MODIS_GDD5_15_", species.name, ".csv")))
dat.midgreen <- read.csv(file.path(path.modis, paste0("MODIS_GDD5_50_", species.name, ".csv")))



#getting paired data
short.bud <- data.frame(YEAR=dat.budburst$year, site = dat.budburst$site_id,
                 bud.YDAY=dat.budburst$first.min,
                 bud.GDD5.cum=dat.budburst$MinGDD5.cum,
                 stringsAsFactors=FALSE)
head(short.bud)

short.leaf <- data.frame(YEAR=dat.leaves$year, site = dat.leaves$site_id,
                        leaf.YDAY=dat.leaves$first.min,
                        leaf.GDD5.cum=dat.leaves$MinGDD5.cum,
                        stringsAsFactors=FALSE)
head(short.leaf)

short.green <- data.frame(YEAR=dat.green$greenup.year, site = dat.green$site, 
                         MODIS15.YDAY=dat.green$greenup.yday,
                         MODIS15.GDD5.cum=dat.green$GDD5.cum,
                         stringsAsFactors=FALSE)
tail(short.green)

short.midgreen <- data.frame(YEAR=dat.midgreen$greenup.year, site = dat.midgreen$site,
                          MODIS50.YDAY=dat.midgreen$greenup.yday,
                          MODIS50.GDD5.cum=dat.midgreen$GDD5.cum,
                          stringsAsFactors=FALSE)
tail(short.midgreen)


#THIS WILL ALL HELP WITH FIGURE 1B
#binding the new dataframes
All.NPN <- merge(short.bud, short.leaf, by=c("site", "YEAR"))
All.MODIS <- merge(short.green, short.midgreen, by=c("site", "YEAR"))
All.dat <- merge(All.NPN, All.MODIS, by=c("site", "YEAR"))
head(All.dat)

#1 date of "Spring"
ggplot(data = All.dat) +
  geom_point(mapping = aes(x = bud.GDD5.cum, y = MODIS15.GDD5.cum))


#This will help with figure 1A
All.long <- reshape::melt(All.dat, id=c("site", "YEAR"))

ggplot(data = All.long) +
  geom_point(mapping = aes(x = YEAR, y = value, color = variable))




#saving the processed NPN data which now has a GDD5.cum as well as actual site names.
dat.processed <- '../data_processed/NPN'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')), row.names=F)


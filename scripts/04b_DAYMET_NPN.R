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

leaves.freq <- as.data.frame(table(dat.leaves$site_name))
colnames(leaves.freq) <- c("Site", "Freq")
leaves.freq$longitude <- dat.leaves$longitude[match(leaves.freq$Site, dat.leaves$site_name)]
leaves.freq$latitude <- dat.leaves$latitude[match(leaves.freq$Site, dat.leaves$site_name)]

map.us <- map_data("state")
png(filename= file.path(path.png, paste0('NPN_sitemap_leaves', species.name, '.png')))
ggplot() +
  coord_fixed(1.3) +
  ggtitle("Map of NPN sites where 'Leaves' for Q. alba was observed from 2000-2019") +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=leaves.freq, aes(x=longitude, y=latitude, color = 'red', size = Freq), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()

#----------------------------------

#getting a map of all the NPN sites which collected observation for 'Buds' from 2000 to 2019

budburst.freq <- as.data.frame(table(dat.budburst$site_name))
colnames(budburst.freq) <- c("Site", "Freq")
budburst.freq$longitude <- dat.budburst$longitude[match(budburst.freq$Site, dat.budburst$site_name)]
budburst.freq$latitude <- dat.budburst$latitude[match(budburst.freq$Site, dat.budburst$site_name)]

map.us <- map_data("state")
png(filename= file.path(path.png, paste0('NPN_sitemap_budburst', species.name, '.png')))
ggplot() +
  coord_fixed(1.3) +
  ggtitle("Map of NPN sites where 'budburst' for Q. alba was observed from 2000-2019") +
  geom_polygon(data=map.us, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_point(data=budburst.freq, aes(x=longitude, y=latitude, color = 'red', size = Freq), alpha=0.75) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks = element_blank())
dev.off()


#----------------------conceptual figures------------------------

path.figures <- "../figures"
if(!dir.exists(path.figures)) dir.create(path.figures)

#see what data there is to work with 
summary(dat.budburst)
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

#binding the new dataframes
All.NPN <- merge(short.bud, short.leaf, by=c("site", "YEAR"))
All.MODIS <- merge(short.green, short.midgreen, by=c("site", "YEAR"))
All.dat <- merge(All.NPN, All.MODIS, by=c("site", "YEAR"))
summary(All.dat)

#---------------------Presentation fig. 2------------------------
colnames(short.green) <- c("site", "YEAR", "YDAY", "GDD5.cum")
colnames(short.midgreen) <- c("site", "YEAR", "YDAY", "GDD5.cum")
colnames(short.leaf) <- c("site", "YEAR", "YDAY", "GDD5.cum")
colnames(short.bud) <- c("site", "YEAR", "YDAY", "GDD5.cum")


short.green$Type <- "15% greenup"
short.midgreen$Type <- "50% greenup"
short.leaf$Type <- "leaves"
short.bud$Type <- "Budburst"

dat.long <- rbind(short.bud, short.leaf, short.green, short.midgreen)

# Doing a quick graph:
day.labels <- data.frame(Date=seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
summary(day.labels)

png("figures/LeafPhenophases.png", height =4, width=6, units="in", res=180)
ggplot(data=dat.long) +
  ggtitle("The Morton Arboretum, Oak Collection") + 
  facet_grid(~Type) +
  geom_histogram(aes(x=YDAY, y=..count.., fill = Type), binwidth=7)+ 
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2,12, by=3)], label=day.labels$Text[seq(2,12, by=3)]) +
  scale_y_continuous(name="# Observations", expand=c(0,0)) +
  guides(fill=F) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=-45, hjust=0)) 
dev.off()


#----------------------concept. fig. 1A -------------------------

#get color blind friendly colors
#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics YDAY
png(width= 750, filename= file.path(path.figures, paste0('Concept1A_BLB_YDAY_', species.name, '.png')))
ggplot(data = All.dat) +
  geom_point(mapping = aes(x = YEAR, y = bud.YDAY, color = 'NPN')) +
  geom_point(mapping = aes(x = YEAR, y = MODIS15.YDAY, color = 'MODIS Greenup')) +
  geom_smooth(mapping = aes(x = YEAR, y = bud.YDAY, color = 'NPN', fill = 'NPN' )) +
  geom_smooth(mapping = aes(x = YEAR, y = MODIS15.YDAY, color = 'MODIS Greenup', fill = 'MODIS Greenup')) +
  scale_y_continuous('Cumulative TT (YDAY)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  ggtitle('Input YDAY Threshold for NPN Breaking Leaf Buds and MODIS 15% Greenup of Quercus alba from 2009-2019') +
  theme(text = element_text(size=10))
dev.off()

#figure using NPN Leaves and MODIS MidGreenup metrics YDAY
png(width= 750, filename= file.path(path.figures, paste0('Concept1A_Leaves_YDAY_', species.name, '.png')))
ggplot(data = All.dat) +
  geom_point(mapping = aes(x = YEAR, y = leaf.YDAY, color = 'NPN Leaves')) +
  geom_point(mapping = aes(x = YEAR, y = MODIS50.YDAY, color = 'MODIS MidGreenup')) +
  geom_smooth(mapping = aes(x = YEAR, y = leaf.YDAY, color = 'NPN Leaves', fill = 'NPN Leaves' )) +
  geom_smooth(mapping = aes(x = YEAR, y = MODIS50.YDAY, color = 'MODIS MidGreenup', fill = 'MODIS MidGreenup')) +
  scale_y_continuous('Cumulative TT (YDAY)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  ggtitle('Input YDAY Threshold for NPN Leaves and MODIS 50% MidGreenup of Quercus alba from 2009-2018') +
  theme(text = element_text(size=10))
dev.off()

#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics GDD5
png(width= 750, filename= file.path(path.figures, paste0('Concept1A_BLB_GDD5_', species.name, '.png')))
ggplot(data = All.dat) +
  geom_point(mapping = aes(x = YEAR, y = bud.GDD5.cum, color = 'NPN Breaking Leaf Buds')) +
  geom_point(mapping = aes(x = YEAR, y = MODIS15.GDD5.cum, color = 'MODIS Greenup')) +
  geom_smooth(mapping = aes(x = YEAR, y = bud.GDD5.cum, color = 'NPN Breaking Leaf Buds', fill = 'NPN Breaking Leaf Buds' )) +
  geom_smooth(mapping = aes(x = YEAR, y = MODIS15.GDD5.cum, color = 'MODIS Greenup', fill = 'MODIS Greenup')) +
  scale_y_continuous('Cumulative TT (GDD5)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  ggtitle('Input GDD5 Threshold for NPN Breaking Leaf Buds and MODIS 15% Greenup of Quercus alba from 2009-2018') +
  theme(text = element_text(size=10))
dev.off()

#figure using NPN Leaves and MODIS MidGreenup metrics GDD5
png(width= 750, filename= file.path(path.figures, paste0('Concept1A_Leaves_GDD5_', species.name, '.png')))
ggplot(data = All.dat) +
  geom_point(mapping = aes(x = YEAR, y = leaf.GDD5.cum, color = 'NPN Leaves')) +
  geom_point(mapping = aes(x = YEAR, y = MODIS50.GDD5.cum, color = 'MODIS MidGreenup')) +
  geom_smooth(mapping = aes(x = YEAR, y = leaf.GDD5.cum, color = 'NPN Leaves', fill = 'NPN Leaves' )) +
  geom_smooth(mapping = aes(x = YEAR, y = MODIS50.GDD5.cum, color = 'MODIS MidGreenup', fill = 'MODIS MidGreenup')) +
  scale_y_continuous('Cumulative TT (GDD5)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  ggtitle('Input GDD5 Threshold for NPN Leaves and MODIS 50% MidGreenup of Quercus alba from 2009-2018') +
  theme(text = element_text(size=10))
dev.off()

#----------------------concept. fig. 1B ------------------------

#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics YDAY
png(width= 750, filename= file.path(path.figures, paste0('Concept1B_BLB_YDAY_', species.name, '.png')))
ggplot(All.dat) +
  coord_equal() +
  geom_point(mapping = aes(x = bud.YDAY, y = MODIS15.YDAY)) +
  geom_smooth(mapping = aes(x = bud.YDAY, y = MODIS15.YDAY), method= 'lm')
dev.off()

Mod.npn1 <- lm(MODIS15.YDAY ~ bud.YDAY, data=All.dat)
summary(Mod.npn1) #statistical summary of the correlation

#figure using NPN Leaves and MODIS MidGreenup metrics YDAY
png(width= 750, filename= file.path(path.figures, paste0('Concept1B_Leaves_YDAY_', species.name, '.png')))
ggplot(All.dat) +
  coord_equal() +
  geom_point(mapping = aes(x = leaf.YDAY, y = MODIS50.YDAY)) +
  geom_smooth(mapping = aes(x = leaf.YDAY, y = MODIS50.YDAY), method= 'lm')
dev.off()

Mod.npn2 <- lm(MODIS50.YDAY ~ leaf.YDAY, data=All.dat)
summary(Mod.npn2) #statistical summary of the correlation

#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics GDD5
png(width= 750, filename= file.path(path.figures, paste0('Concept1B_BLB_GDD5_', species.name, '.png')))
ggplot(All.dat) +
  coord_equal() +
  geom_point(mapping = aes(x = bud.GDD5.cum, y = MODIS15.GDD5.cum)) +
  geom_smooth(mapping = aes(x = bud.GDD5.cum, y = MODIS15.GDD5.cum), method= 'lm')
dev.off()

Mod.npn3 <- lm(MODIS15.GDD5.cum ~ bud.GDD5.cum, data=All.dat)
summary(Mod.npn3) #statistical summary of the correlation

#figure using NPN Leaves and MODIS MidGreenup metrics GDD5
png(width= 750, filename= file.path(path.figures, paste0('Concept1B_Leaves_GDD5_', species.name, '.png')))
ggplot(All.dat) +
  coord_equal() +
  geom_point(mapping = aes(x = leaf.GDD5.cum, y = MODIS50.GDD5.cum)) +
  geom_smooth(mapping = aes(x = leaf.GDD5.cum, y = MODIS50.GDD5.cum), method= 'lm')
dev.off()

Mod.npn4 <- lm(MODIS50.GDD5.cum ~ leaf.GDD5.cum, data=All.dat)
summary(Mod.npn4) #statistical summary of the correlation

#--------------------concept. fig. 2---------------------------

#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics
png(width= 750, filename= file.path(path.figures, paste0('Concept2_BLB_', species.name, '.png')))
ggplot(data= All.dat) +
  ggtitle('Input GDD5 Threshold for NPN Breaking Leaf Buds and MODIS Greenup of Quercus alba') +
  geom_density(mapping = aes(x = bud.GDD5.cum, color = 'NPN Breaking Leaf Buds', fill = 'NPN Breaking Leaf Buds'), alpha = 0.5) +
  geom_density(mapping = aes(x = MODIS15.GDD5.cum, color = 'MODIS Greenup', fill = 'MODIS Greenup'), alpha = 0.5) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (Probability)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  theme(text = element_text(size=10))
dev.off()

#figure using NPN Leaves and MODIS MidGreenup metrics
png(width= 750, filename= file.path(path.figures, paste0('Concept2_Leaves_', species.name, '.png')))
ggplot(data= All.dat) +
  ggtitle('Input GDD5 Threshold for NPN Leaves and MODIS MidGreenup of Quercus alba') +
  geom_density(mapping = aes(x = leaf.GDD5.cum, color = 'NPN Leaves', fill = 'NPN Leaves'), alpha = 0.5) +
  geom_density(mapping = aes(x = MODIS50.GDD5.cum, color = 'MODIS MidGreenup', fill = 'MODIS MidGreenup'), alpha = 0.5) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('DENSITY (Probability)') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  theme(text = element_text(size=10))
dev.off()

#--------------------concept. fig. 3--------------------------

#figure using NPN Breaking Leaf Buds and MODIS Greenup metrics
png(width= 750, filename= file.path(path.figures, paste0('Concept3_BLB_', species.name, '.png')))
ggplot(data= All.dat) +
  ggtitle('Input GDD5 Threshold and YDAY for NPN Breaking Leaf Buds and MODIS Greenup of Quercus alba') +
  geom_smooth(mapping = aes(x = bud.GDD5.cum, y = bud.YDAY, color = 'NPN Breaking Leaf Buds', fill = 'NPN Breaking Leaf Buds')) +
  geom_smooth(mapping = aes(x = MODIS15.GDD5.cum, y = MODIS15.YDAY, color = 'MODIS Greenup', fill = 'MODIS Greenup')) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('YDAY') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  theme(text = element_text(size=10))
dev.off()

#figure using NPN Leaves and MODIS MidGreenup metrics
png(width= 750, filename= file.path(path.figures, paste0('Concept3_Leaves_', species.name, '.png')))
ggplot(data= All.dat) +
  ggtitle('Input GDD5 Threshold and YDAY for NPN Leaves and MODIS MidGreenup of Quercus alba') +
  geom_smooth(mapping = aes(x = leaf.GDD5.cum, y = leaf.YDAY, color = 'NPN Leaves', fill = 'NPN Leaves')) +
  geom_smooth(mapping = aes(x = MODIS50.GDD5.cum, y = MODIS50.YDAY, color = 'MODIS MidGreenup', fill = 'MODIS MidGreenup')) +
  scale_x_continuous('TT Threshold (5C Growing Degree Days)') +
  scale_y_continuous('YDAY') +
  scale_fill_manual(name='Metric', values=c("orange", "forestgreen")) +
  scale_color_manual(name='Metric', values=c("orange", "forestgreen")) +
  theme(text = element_text(size=10))
dev.off()

#-------------------------------------
#saving the processed NPN data which now has a GDD5.cum as well as actual site names.
dat.processed <- '../data_processed/NPN'
if(!dir.exists(dat.processed)) dir.create(dat.processed)
write.csv(dat.budburst, file.path(dat.processed, paste0('Quercus_bud_GDD5_', species.name, '_NPN.csv')), row.names=F)
write.csv(dat.leaves, file.path(dat.processed, paste0('Quercus_leaf_GDD5_', species.name, '_NPN.csv')), row.names=F)


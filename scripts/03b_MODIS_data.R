#data from MODIS, MCD12Q2 found at https://lpdaac.usgs.gov/products/mcd12q2v006/
#this is the extraction of the raw MODIS data for the bands 15% Greenup and 50% MidGreenup
#------------------------

#install.packages("MODISTools")
library(MODISTools)

species.name <- 'Q.alba'
#----------------------------------
#bringing in the Raw NPN data to pull the site and coordinate data out
path.NPN <- "../data_raw/NPN/uncleaned"

species.name = 'Q.alba'

oak.bud <- read.csv(file.path(path.clean, paste0('NPN_Quercus_bud_', species.name, '.csv')))
oak.leaf <- read.csv(file.path(path.clean, paste0('NPN_Quercus_leaf_', species.name, '.csv')))

oak.both <- rbind(oak.leaf, oak.bud)

dim(oak.both)

# Creating a point list and time range that matches your MODIS dataset
NPN.pts <- aggregate(site_id~latitude+longitude, data=oak.both, FUN=min)

head(NPN.pts)
names(NPN.pts)[1] <- 'lat'
names(NPN.pts)[2] <- 'lon'
names(NPN.pts)[3] <- 'site_name'
NPN.pts <- NPN.pts[,c(3,1,2)]

summary(NPN.pts)
#fixing coordinates that omitted the "-" in front of the longitude
NPN.pts$longitude[NPN.pts$longitude>0] <- NPN.pts$longitude[NPN.pts$longitude>0]*-1

#showing only the bands of interest within the product 'MCD12Q2
mtbands <- MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!

leaf.MODIS <- MODISTools::mt_batch_subset(df= NPN.pts, product= 'MCD12Q2', band= 'Greenup.Num_Modes_01', start= '1999-01-01', end= '2019-12-31')
summary(leaf.MODIS)


midgreen.MODIS <- MODISTools::mt_batch_subset(df= NPN.pts, product= 'MCD12Q2', band= 'MidGreenup.Num_Modes_01', start= '1999-01-01', end= '2019-12-31')
summary(midgreen.MODIS)

leaf.MODIS$value[leaf.MODIS$value == 32767] <- NA #value representative of NA in MODIS, this filters that out
midgreen.MODIS$value[midgreen.MODIS$value == 32767] <- NA 

#some formatting that will change the band names and alter the display of the dates
leaf.MODIS$calendar_date <- as.Date(leaf.MODIS$calendar_date)
leaf.MODIS$band <- as.factor(leaf.MODIS$band)
leaf.MODIS$BAND <- as.factor(sapply(leaf.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#some formatting that will change the band names and alter the display of the dates
midgreen.MODIS$calendar_date <- as.Date(midgreen.MODIS$calendar_date)
midgreen.MODIS$band <- as.factor(midgreen.MODIS$band)
midgreen.MODIS$BAND <- as.factor(sapply(midgreen.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
leaf.MODIS$value_date <- as.Date('1970-01-01') + leaf.MODIS$value
summary(leaf.MODIS)

#convert the days since 1970 value to a calendar date
midgreen.MODIS$value_date <- as.Date('1970-01-01') + midgreen.MODIS$value
summary(midgreen.MODIS)

leaf.MODIS$greenup.year <- lubridate::year(leaf.MODIS$value_date)
leaf.MODIS$greenup.yday <- lubridate::yday(leaf.MODIS$value_date)

midgreen.MODIS$greenup.year <- lubridate::year(midgreen.MODIS$value_date)
midgreen.MODIS$greenup.yday <- lubridate::yday(midgreen.MODIS$value_date)

#putting a filter on the DOY vaue of MODIS so that we are still looking at the spring season
summary(leaf.MODIS$greenup.yday)
leaf.MODIS[leaf.MODIS$greenup.yday>182 & !is.na(leaf.MODIS$greenup.yday), "greenup.yday"] <- NA
summary(leaf.MODIS$greenup.yday)

summary(midgreen.MODIS$greenup.yday)
leaf.MODIS[midgreen.MODIS$greenup.yday>182 & !is.na(midgreen.MODIS$greenup.yday), "greenup.yday"] <- NA
summary(midgreen.MODIS$greenup.yday)

#visual showing the DOY that the 15% Greenup happened at each of the sites included from NPN.pts for the years 2000-2019
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('MODIS_Greenup_YDAY_', species.name, '.png')))

hist(leaf.MODIS$greenup.yday)

dev.off()

#visual showing the DOY that the 50% MidGreenup happened at each of the sites included from NPN.pts for the years 2000-2019
png(filename= file.path(path.png, paste0('MODIS_MidGreenup_YDAY_', species.name, '.png')))

hist(midgreen.MODIS$greenup.yday)

dev.off()
#--------------------------------------

#translate yday to calendar dates
days.mrk <- data.frame(Label <- c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'),
                      Date <- c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)

#saving the filtered and downloaded MODIs data for all sites of NPN data for Q. alba 'Breaking Leaf Buds' and 'Leaves'
path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)
write.csv(leaf.MODIS, file.path(path.MODIS, paste0("MODIS_Greenup_Quercus_", species.name, ".csv")), row.names=F)
write.csv(midgreen.MODIS, file.path(path.MODIS, paste0("MODIS_MidGreenup_Quercus_", species.name, ".csv")), row.names=F)

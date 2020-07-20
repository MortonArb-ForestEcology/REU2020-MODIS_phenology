#data from MODIS, MCD12Q2 found at https://lpdaac.usgs.gov/products/mcd12q2v006/
#this is the extraction of the raw MODIS data for the bands 15% Greenup and 50% MidGreenup
#------------------------

#install.packages("MODISTools")
library(MODISTools)
#install.packages("ggplot2")
library(ggplot2)

species.name <- 'Q.alba'

path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
dat.leaves <- read.csv(file.path(path.clean, paste0("NPN_Quercus_leaf_", species.name, ".csv")))

#----------------------------------
#LNPN sites greenup

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
NPN.pts <- read.csv(file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")))

summary(NPN.pts)

NPN.pts <- aggregate(site.id~latitude+longitude, data=dat.leaves, FUN=min)

head(NPN.pts)
names(NPN.pts)[1] <- 'lat'
names(NPN.pts)[2] <- 'lon'
names(NPN.pts)[3] <- 'site_name'
NPN.pts <- NPN.pts[,c(3,1,2)]

summary(NPN.pts)

#showing only the bands of interest within the product 'MCD12Q2
mtbands <- MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!

leaf.MODIS <- MODISTools::mt_batch_subset(df= NPN.pts, product= 'MCD12Q2', band= 'Greenup.Num_Modes_01', start= '1999-01-01', end= '2019-12-31')
summary(leaf.MODIS)


leaf.MODIS$value[leaf.MODIS$value == 32767] <- NA #value representative of NA in MODIS, this filters that out

#some formatting that will change the band names and alter the display of the dates
leaf.MODIS$calendar_date <- as.Date(leaf.MODIS$calendar_date)
leaf.MODIS$band <- as.factor(leaf.MODIS$band)
leaf.MODIS$BAND <- as.factor(sapply(leaf.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
leaf.MODIS$value_date <- as.Date('1970-01-01') + leaf.MODIS$value

summary(leaf.MODIS)

leaf.MODIS$greenup.year <- lubridate::year(leaf.MODIS$value_date)
leaf.MODIS$greenup.yday <- lubridate::yday(leaf.MODIS$value_date)

leaf.MODIS[leaf.MODIS$greenup.yday>182 & !is.na(leaf.MODIS$greenup.yday), "greenup.yday"] <- NA

hist(leaf.MODIS$greenup.yday)

#--------------------------------------

#translate yday to calendar dates
days.mrk <- data.frame(Label <- c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'),
                      Date <- c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)

# Storing the raw MODIS output 
path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)
write.csv(leaf.MODIS, file.path(path.MODIS, paste0("MODIS_Greenup_Quercus_", species.name, ".csv")), row.names=F)

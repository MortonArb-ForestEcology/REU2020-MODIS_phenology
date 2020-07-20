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
dat.budburst <- read.csv(file.path(path.clean, paste0("NPN_Quercus_bud_", species.name, ".csv")))
dat.leaves <- read.csv(file.path(path.clean, paste0("NPN_Quercus_leaf_", species.name, ".csv")))

#----------------------------------
#Leaves sites greenup
leaf.pts <- aggregate(site.id~latitude+longitude, data=dat.leaves,
                     FUN=min)
# path.DAYMET <- '../data_raw/DAYMET'
# npn.pts <- read.csv(file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")))
# names(npn.pts)

# leaf.pts <- npn.pts[,c("site_id", "latitude", "longitude")]
# names(leaf.pts) <- c("site_name", "lat", "lon")
# head(leaf.pts)
names(leaf.pts)[1] <- 'lat'
names(leaf.pts)[2] <- 'lon'
names(leaf.pts)[3] <- 'site_name'

#showing only the bands of interest within the product 'MCD12Q2
mtbands <- MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!

greenup.out <- list()
for(i in 1:nrow(leaf.pts)){
  greenup.out[[i]] <- MODISTools::mt_subset(product="MCD12Q2", band="Greenup.Num_Modes_01", start= '1999-01-01', end= '2019-12-31', lat = leaf.pts$lat[i], lon=leaf.pts$lon[i], site_name = leaf.pts$site_name[i])
}
# leaf.MODIS <- MODISTools::mt_batch_subset(df= leaf.pts[1:5,], product= 'MCD12Q2', band= 'Greenup.Num_Modes_01', start= '1999-01-01', end= '2019-12-31', ncores=4)
# head(leaf.MODIS)
leaf.MODIS <- dplyr::bind_rows(greenup.out)
summary(leaf.MODIS)


leaf.MODIS$value[leaf.MODIS$value == 32767] <- NA #value representative of NA in MODIS, this filters that out

#some formatting that will change the band names and alter the display of the dates
leaf.MODIS$calendar_date <- as.Date(leaf.MODIS$calendar_date)
leaf.MODIS$band <- as.factor(leaf.MODIS$band)
leaf.MODIS$BAND <- as.factor(sapply(leaf.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
leaf.MODIS$value_date <- as.Date('1970-01-01') + leaf.MODIS$value

head(leaf.MODIS)

leaf.MODIS$greenup.year <- lubridate::year(leaf.MODIS$value_date)
leaf.MODIS$greenup.yday <- lubridate::yday(leaf.MODIS$value_date)

hist(leaf.MODIS$greenup.yday)

#-------------------------------------
#bud burst sites greenup

budburst.pts <- aggregate(site.id~latitude+longitude, data=dat.budburst,
                          FUN=min)
head(budburst.pts)
names(budburst.pts)[1] <- 'lat'
names(budburst.pts)[2] <- 'lon'
names(budburst.pts)[3] <- 'site_name'

# budburst.MODIS <- MODISTools::mt_batch_subset(df= budburst.pts, product= 'MCD12Q2', band= 'Greenup.Num_Modes_01', start= '1999-01-01', end= '2019-12-31')
# head(budburst.MODIS)
greenup.out <- list()
for(i in 1:nrow(leaf.pts)){
  greenup.out[[i]] <- MODISTools::mt_subset(product="MCD12Q2", band="Greenup.Num_Modes_01", start= '1999-01-01', end= '2019-12-31', lat = budburst.pts$lat[i], lon=budburst.pts$lon[i], site_name = budburst.pts$site_name[i])
}
# head(leaf.MODIS)
leaf.MODIS <- dplyr::bind_rows(greenup.out)
summary(leaf.MODIS)


budburst.MODIS$value[budburst.MODIS$value == 32767] <- NA #value representative of NA in MODIS, this filters that out

#some formatting that will change the band names and alter the display of the dates
budburst.MODIS$calendar_date <- as.Date(budburst.MODIS$calendar_date)
budburst.MODIS$band <- as.factor(budburst.MODIS$band)
budburst.MODIS$BAND <- as.factor(sapply(budburst.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
budburst.MODIS$value_date <- as.Date('1970-01-01') + budburst.MODIS$value

head(budburst.MODIS)

budburst.MODIS$greenup.year <- lubridate::year(budburst.MODIS$value_date)
budburst.MODIS$greenup.yday <- lubridate::yday(budburst.MODIS$value_date)

hist(budburst.MODIS$greenup.yday)
#--------------------------------------

#translate yday to calendar dates
days.mrk <- data.frame(Label <- c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'),
                      Date <- c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)

# Storing the raw MODIS output 
path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)
write.csv(leaf.MODIS, file.path(path.MODIS, paste0("MODIS_Greenup_leaf_", species.name, ".csv")), row.names=F)
write.csv(budburst.MODIS, file.path(path.MODIS, paste0("MODIS_Greenup_bud_", species.name, ".csv")), row.names=F)

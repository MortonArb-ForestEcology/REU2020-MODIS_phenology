#data from MODIS, MCD12Q2 found at https://lpdaac.usgs.gov/products/mcd12q2v006/
#this is the extraction of the raw MODIS data for the bands 15% Greenup and 50% MidGreenup
#------------------------

#install.packages("MODISTools")
library(MODISTools)
#install.packages("ggplot2")
library(ggplot2)

site.id <- 'MortonArb'
lat.in <- 41.813583
lon.in <- -88.052897

#showing only the bands of interest within the product 'MCD12Q2
mtbands <- MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

#get list of dates for greenup
dat.d8 <- MODISTools::mt_dates('MCD12Q2', lat = lat.in, lon = lon.in)
summary(dat.d8) #see the temporal range of available data for this area. Should be from 2001 - 2017

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!
#focusing on the variables in question, Here is it Greenup.Num_Modes_01
dat.MODIS <- MODISTools::mt_subset(product = 'MCD12Q2', band=c('Greenup.Num_Modes_01', 'MidGreenup.Num_Modes_01'), lat = lat.in, lon=lon.in, start= '1995-01-01', site_name = site.id)
head(dat.MODIS)

dat.MODIS$value[dat.MODIS$value == 32767] <- NA #value representative of NA in MODIS, this filters that out

#some formatting that will change the band names and alter the display of the dates
dat.MODIS$calendar_date <- as.Date(dat.MODIS$calendar_date)
dat.MODIS$band <- as.factor(dat.MODIS$band)
dat.MODIS$BAND <- as.factor(sapply(dat.MODIS$band, FUN <- function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
dat.MODIS$value_date <- as.Date('1970-01-01') + dat.MODIS$value

head(dat.MODIS)

dat.MODIS$greenup.year <- lubridate::year(dat.MODIS$value_date)
dat.MODIS$greenup.yday <- lubridate::yday(dat.MODIS$value_date)

#translate yday to calendar dates
days.mrk <- data.frame(Label <- c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'),
                      Date <- c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)

#simple plot that should show the data for this area greening up between 2005 and 2015.
figures.dat <- '../figures'
if(!dir.exists(figures.dat)) dir.create(figures.dat)
png(width= 750, filename= file.path(figures.dat, paste0('Greenup_Plot_MODIS_', site.id, '.png')))
ggplot(data = dat.MODIS, mapping = aes(x= greenup.year, y= greenup.yday)) +
  ggtitle('MODIS Greenup and Midgreenup') +
  geom_line(mapping= aes(color= BAND)) +
  geom_point(color= 'orange', size= .5) + 
  geom_text(label= dat.MODIS$greenup.yday, color= 'black', size = 5) +
  scale_x_continuous('YEAR', breaks= seq(from= '1995', to='2020', by= 1 )) +
  scale_y_continuous('DATE (YDay)', breaks= days.mrk$mrk.yday, labels= days.mrk$Label)
dev.off() 

# Storing the raw MODIS output 
path.MODIS <- '../data_raw/MODIS'
if(!dir.exists(path.MODIS)) dir.create(path.MODIS)
write.csv(dat.MODIS, file.path(path.MODIS, paste0("MODIS_Greenup_Raw_", site.id, ".csv")), row.names=F)

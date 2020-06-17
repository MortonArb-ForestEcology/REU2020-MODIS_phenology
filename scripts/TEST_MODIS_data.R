# Andrew's attempt at recreating the Test_MODIS code that Christy pushed
#data from MODIS, MCD12Q2 found at https://lpdaac.usgs.gov/products/mcd12q2v006/
#----------------
#install.packages("MODISTools")
library(MODISTools)
#install.packages("tidyverse")
library(tidyverse)

#coordinates found from Google Earth of Andrew's Ivy Branch Farm
site.id = 'IvyBranchFarm'
lat.in= 39.788276
lon.in= -94.010451

#set the data that we want specifically from MODISTools to prods
prods= MODISTools::mt_products()
summary(prods)
prods$product
prods[prods$product == 'MCD12Q2',]

#showing only the bands of interest within the product 'MCD12Q2
mtbands = MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

#get list of dates for greenup
dat.d8 = MODISTools::mt_dates('MCD12Q2', lat = lat.in, lon = lon.in)
dat.d8 #see the temporal range of available data for this area. Should be from 2001 - 2017

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!
#focusing on the variables in question, Here is it Greenup.Num_Modes_01
dat.tst = MODISTools::mt_subset(product= 'MCD12Q2', band=c('Greenup.Num_Modes_01'), lat=lat.in, lon=lon.in, start= '2005-01-01', end= '2015-01-01', site_name= site.id)
head(dat.tst)
dat.tst

dat.tst$value[dat.tst$value == 32767] = NA #value representative of NA in MODIS, this filters that out

#some formatting that will aestically change the band names and alter the display of the dates
dat.tst$calendar_date = as.Date(calendar_date)
dat.tst$band = as.factor(dat.tst$band)
dat.tst$band.name = as.factor(sapply(dat.tst$band, FUN =function(x){stringr::str_split(x, '[.]'), [1][1]})) #I somewhat understand it, but the explainations for using sapply confuse me.

#convert the days since 2001-01-01 value to a calendar date
dat.tst$value_date = as.Date('1970-01-01') + dat.tst$value #not sure exactly how this one works

summary(dat.tst)
head(dat.tst)
dat.tst #at this point when ran, the bottom values =0 Not sure why

dat.tst$greenup.year = lubridate::year(dat.tst$value.date) #is the .date telling it how to format the value? whats the difference from that and _date
dat.tst$greenup.yday = lubridate::yday(dat.tst$value.date)

#translate yday to calendar dates
days.mrk = data.frame(Label= c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'), #Is this just to specify our speculated timeframe? What happens if it is wrong?
                      Date= c('2019-01-01', '2019-02-01,' '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)
days.mrk$mrk.yday <- days.mrk$mrk.yday #why "1 = 1"?

#simple plot that should show the data for this farm greening up between 2005 and 2015.
ggplot(data=dat.tst) +
  geom_point() +
  geom_contour() +
  scale_x_continuous('Year') +
  scale_y_continuous('Day',breaks = days.mrk$mrk.ydays, labels = days.mrk$Label) #why does the days.mrk still need included? why not labels = Label


# Storing the raw MODIS output 
if(!dir.exists("../data_raw/MODIS")) dir.create("../data_raw/MODIS")
write.csv(dat.test, "../data_raw/MODIS/TEST_Greenup_TheMortonArboretum.csv", row.names=F)
                      


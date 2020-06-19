# Andrew's attempt at recreating the Test_MODIS code that Christy pushed
#data from MODIS, MCD12Q2 found at https://lpdaac.usgs.gov/products/mcd12q2v006/
#----------------
#install.packages("MODISTools")
library(MODISTools)
#install.packages("ggplot2")
library(ggplot2)

#coordinates found from Google Earth of Andrew's Ivy Branch Farm
site.id = 'IvyBranchFarm'
lat.in= 39.788276
lon.in= -94.010451

#set the data that we want specifically from MODISTools to prods
prods <- MODISTools::mt_products()
summary(prods)
prods$product
prods[prods$product == 'MCD12Q2',]

#showing only the bands of interest within the product 'MCD12Q2
mtbands <- MODISTools::mt_bands('MCD12Q2')
dim(mtbands)

#get list of dates for greenup
dat.d8 <- MODISTools::mt_dates('MCD12Q2', lat = lat.in, lon = lon.in)
dat.d8 #see the temporal range of available data for this area. Should be from 2001 - 2017

mtbands #choose the variables of interest for the next step, !!!! use only the _01 bands !!!!
#focusing on the variables in question, Here is it Greenup.Num_Modes_01
dat.tst <- MODISTools::mt_subset(product= 'MCD12Q2', band=c('Greenup.Num_Modes_01', 'MidGreenup.Num_Modes_01'), lat=lat.in, lon=lon.in, start= '2005-01-01', end= '2015-01-01', site_name= site.id)
head(dat.tst)
dat.tst

dat.tst$value[dat.tst$value == 32767] <- NA #value representative of NA in MODIS, this filters that out

#some formatting that will change the band names and alter the display of the dates
dat.tst$calendar_date <- as.Date(dat.tst$calendar_date)
dat.tst$band <- as.factor(dat.tst$band)
dat.tst$BAND <- as.factor(sapply(dat.tst$band, FUN= function(x){stringr::str_split(x, '[.]') [[1]][1]}))

#convert the days since 1970 value to a calendar date
dat.tst$value_date <- as.Date('1970-01-01') + dat.tst$value

summary(dat.tst)
head(dat.tst)

dat.tst

dat.tst$greenup.year <- lubridate::year(dat.tst$value_date)
dat.tst$greenup.yday <- lubridate::yday(dat.tst$value_date)

#translate yday to calendar dates
days.mrk <- data.frame(Label= c('Jan 1', 'Feb 1', 'Mar 1', 'Apr1', 'May 1', 'Jun 1', 'Jul 1'),
                      Date= c('2019-01-01', '2019-02-01', '2019-03-01', '2019-04-01', '2019-05-01', '2019-06-01', '2019-07-01'))
days.mrk$mrk.yday <- lubridate::yday(days.mrk$Date)

#simple plot that should show the data for this farm greening up between 2005 and 2015.
ggplot(data= dat.tst, mapping= aes(x= greenup.year, y= greenup.yday)) +
  ggtitle('Ivy Branch Greenup to Maturity Trends From 2005 - 2015') +
  geom_line(mapping= aes(color= BAND)) +
  geom_point(color= 'orange', size= .5) + 
  geom_text(label= dat.tst$greenup.yday, color= 'black', size = 5) +
  scale_x_continuous('YEAR', breaks= seq(from= '2005', to= '2015', by= 1 )) +
  scale_y_continuous('DATE (YDay)', breaks= days.mrk$mrk.yday, labels= days.mrk$Label)

# Storing the raw MODIS output 
if(!dir.exists("../data_raw/MODIS")) dir.create("../data_raw/MODIS")
write.csv(dat.tst, file.path("../data_raw/MODIS/", paste0("TEST_Greenup_", site.id, ".csv")), row.names=F)
png(file='Test_Plot_', site.id, '.png')
plot(dat.tst)

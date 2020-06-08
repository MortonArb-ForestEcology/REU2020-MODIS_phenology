# Trying out using modis tools to get the data we need
# We want the MCD12Q2 product.  
#  Details here: https://lpdaac.usgs.gov/products/mcd12q2v006/

# install.packages("MODISTools")
library(ggplot2)

lat.in=41.812739
lon.in=-88.072749


# library(MODISTools)

# Get list of available MODIS datasets
prods <- MODISTools::mt_products()
summary(prods)
prods$product
prods[prods$product=="MCD12Q2",]

# Get list of available layers (aka "bands").  See link above for description
mdbands <- MODISTools::mt_bands(product="MCD12Q2")

# Get available dates for Greenup
dat.date <- MODISTools::mt_dates("MCD12Q2", lat=lat.in, lon=lon.in)
dat.date # Data from 2001 to 2017

# Trying out getting MODIS Phenology data.  Lets start with onset: Greenup.Num_Modes_01
dat.test <- MODISTools::mt_subset(product="MCD12Q2", band=c("Greenup.Num_Modes_01", "MidGreenup.Num_Modes_01"), lat=lat.in, lon=lon.in, site_name = "MortonArb")
dat.test$value[dat.test$value==32767] <- NA # 32767 is the missing data code

# Doing just a tiny bit of formattings
dat.test$calendar_date <- as.Date(dat.test$calendar_date)
dat.test$band <- as.factor(dat.test$band)
dat.test$band.name <- as.factor(sapply(dat.test$band, FUN=function(x){stringr::str_split(x, "[.]")[[1]][1]}))
summary(dat.test)

# Convert days since Jan 1 1970 to a calendar date
dat.test$value.date <- as.Date("1970-01-01") + dat.test$value
summary(dat.test)

# Extract useful parts of that date
dat.test$greenup.year <- lubridate::year(dat.test$value.date)
dat.test$greenup.yday <- lubridate::yday(dat.test$value.date)


# A handy bit of code that helps me translate yday to caldenar dates
days.mark <- data.frame(Label=c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1"), 
                        Date=c("2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01"))
days.mark$mark.yday <- lubridate::yday(days.mark$Date)
days.mark$mark.yday <- days.mark$mark.yday

# Making a quick exploratory graph
ggplot(data=dat.test, aes(x=greenup.year, y=greenup.yday, color=band.name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name="Day", breaks=days.mark$mark.yday, labels=days.mark$Label) +
  scale_x_continuous(name="Year")


# Storing the raw MODIS output 
if(!dir.exists("../data_raw/MODIS")) dir.create("../data_raw/MODIS")
write.csv(dat.test, "../data_raw/MODIS/TEST_Greenup_TheMortonArboretum.csv", row.names=F)

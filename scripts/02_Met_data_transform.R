#This script will serve to download the daymet weather data for every location we use for a set of years

site.id = 'IvyBranchFarm'

dat.tst <- read.csv(file.path('../data_raw/MODIS', paste0("TEST_Greenup_", site.id, ".csv")))
summary(dat.tst)

#-------------------------------------------------------------------------------------#
#This works for a single point at the arboretum using data we already have downloaded.
#However we are going to want to do this for locations all across the country.
#To do that we are going to need to use the daymetr package to download daymet data
#-------------------------------------------------------------------------------------#
#install.packages("daymetr")


#Setting the points to download the daymet data from
path.daymet <- "../data_raw/DAYMET"
if(!dir.exists(path.daymet)) dir.create(path.daymet)

summary(dat.tst)


# Creating a point list and time range that matches your MODIS dataset
modis.pts <- aggregate(greenup.year~site+latitude+longitude, data=dat.tst, 
                       FUN=min)
names(modis.pts)[4] <- "yr.start"
modis.pts$yr.end <- aggregate(greenup.year~site+latitude+longitude, data=dat.tst, 
                              FUN=max)[,4]
modis.pts


#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(modis.pts, file.path(path.daymet, paste0("TEST_POINTS_", site.id, ".csv")), row.names=FALSE)

# if(!dir.exist(path.daymet)) dir.create(path.daymet)
#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.daymet, "MODIS_TEST_points.csv"),
                                           start = min(modis.pts$yr.start),
                                           end = max(modis.pts$yr.end),
                                           internal = T)


# This gives us a list with one layer per site (I think)
length(lat.list)
class(lat.list[[1]])
summary(lat.list[[1]])
summary(lat.list[[1]][["data"]]) # Lets us look at the data for the first site


#Lets look at the structure of what we are given
summary(lat.list)

#So summary might look weird so lets try str()
str(lat.list)

#As you might notice we have a "List of 1" containing a nested "List of 7"
#Within that "List of 7" there is a data frame called data that has the data we want
#Well we know where it is now but how do we access it?

#This is the general format
lat.list[[1]]$latitude

#If you are reaching further into the data
lat.list[[1]]$data$yday


#-----------------------------------------------------------------------#
#This is where we start preparing for the loop for GDD5 calculation.
#-----------------------------------------------------------------------#
#This package is needed for the %>% pipe used in the loop to summarise data
library(dplyr)

#Creating a dataframe to hold weather summary statistics
#This step is needed because we can not fill an empty data frame. This gives us an object for us to fill over the loop
#This fills a data frame with a row for latitude and longitude for every year in our range
df.loc <- data.frame(latitude=rep(lat.list[[1]]$latitude, 365* ((yend-ystart)+1)) ,
                     longitude=rep(lat.list[[1]]$longitude, 365* ((yend-ystart)+1)))

#Making sure we only go through relevant years we are calculating gdd5 for
dat.tst <- dat.tst[dat.tst$Year >= ystart, ] #will this still work if the dat.tst is constrained to 2005-2015? dat.tst can be easily opened up though

#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 1
i <- 1
YR <- ystart #will ystart be an input point for us when trying to visual data?
for(i in seq_along(lat.list)){
  #df.tmp is all weather data for a location
  df.tmp <- lat.list[[i]]$data
  df.tmp$TMEAN <- (df.tmp$tmax..deg.c. + df.tmp$tmin..deg.c.)/2
  df.tmp$GDD5 <- ifelse(df.tmp$TMEAN>5, df.tmp$TMEAN-5, 0)
  df.tmp$GDD5.cum <- NA
  
  #Loop that goes through every year for each point
  for(YR in min(df.tmp$year):max(df.tmp$year)){
    #df.yr is all weather data for a year at a location
    df.yr <- df.tmp[df.tmp$year==YR,]
    
    #Creating dates to correspond with the yday
    start <- paste(as.character(df.tmp$year), "-01-01", sep="")
    df.tmp$Date <- as.Date((df.yr$yday-1), origin = start)
    
    #This should look like the same loop we used earlier
    gdd.cum=0
    d.miss = 0
    for(j in 1:nrow(df.yr)){
      if(is.na(df.yr$GDD5[j]) & d.miss<=3){
        d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
        gdd.cum <- gdd.cum+0
      } else {
        d.miss = 0 # reset to 0
        gdd.cum <- gdd.cum+df.yr$GDD5[j] 
      }
      df.yr[j,"GDD5.cum"] <- gdd.cum
    }
    df.tmp[df.tmp$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
    
  } #what is the purpose of this bracket and the one on line 212? 
  df.loc$"latitude" <- lat.list[[i]]$latitude
  df.loc$"longitude" <- lat.list[[i]]$longitude
  df.loc$"year" <- df.tmp$year
  df.loc$"TMAX" <- df.tmp$tmax..deg.c.
  df.loc$"TMIN" <- df.tmp$tmin..deg.c.
  df.loc$"TMEAN" <- df.tmp$TMEAN
  df.loc$"PRCP" <- df.tmp$prcp..mm.day.
  df.loc$"SNOW" <- df.tmp$srad..W.m.2.
  df.loc$"YDAY" <- df.tmp$yday
  df.loc$"Date" <- df.tmp$Date
  df.loc$"GDD5" <- df.tmp$GDD5
  df.loc$"GDD5.cum" <- df.tmp$GDD5.cum
}

#need save path?


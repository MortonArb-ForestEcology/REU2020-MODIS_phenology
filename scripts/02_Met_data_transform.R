#This script will serve to download the daymet weather data for every location we use for a set of years




path.g <- "G:/My Drive"
#path.g <- "/Volumes/GoogleDrive/My Drive"
#path.g <- "Your/filepath/here"
#-------------------------------------------------#
#This section is for downloaded the met data, pulling out data of interest, and calculating growing degree days
#-------------------------------------------------#

#Setting a shared file path for where the data are
path.met <- file.path(path.g, "Arboretum Met Data/GHCN-Daily")

# Read in the older dataset. This is because the GHCND at the arboretum changed in 2007 and we need to pull from both
met.old <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00119221_1895-2007.csv"))
met.old$DATE <- as.Date(met.old$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.old)

# Read in the newer dataset
met.new <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00115097_2007-04-01_2019-12-31.csv"))
met.new$DATE <- as.Date(met.new$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.new)

# Check to make sure we're in metric (earlier I had a mixture of units and that was bad)
range(met.old$TMAX, na.rm=T)
range(met.new$TMAX, na.rm=T)


# Combine the old and the new datasets into a new data frame.  We don't want all columns, so just take the ones we care about
met.all <- rbind(met.old[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")],
                 met.new[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")])

summary(met.all)

#Pulling out useful date values
met.all$YEAR <- lubridate::year(met.all$DATE)
met.all$MONTH <- lubridate::month(met.all$DATE)
met.all$DAY <- lubridate::day(met.all$DATE)
met.all$YDAY <- lubridate::yday(met.all$DATE)

#Setting our chosen years
met.all <- met.all[met.all$YEAR>1995 & met.all$YEAR<2020,]

#Adding TMEAN
met.all$TMEAN <- (met.all$TMAX + met.all$TMIN)/2


summary(met.all)

# Adding in growing degree-days with base temp of 5
met.all$GDD5 <- ifelse(met.all$TMEAN>5, met.all$TMEAN-5, 0)
met.all$GDD5.cum <- NA
summary(met.all)


# Calculate the cumulative growing degree days for each day/year
for(YR in unique(met.all$YEAR)){
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  gdd5.cum=0 
  d5.miss = 0
  for(i in 1:nrow(dat.tmp)){
    if(is.na(dat.tmp$GDD5[i])){ 
      gdd5.cum <- gdd5.cum+0
    } else {
      gdd5.cum <- gdd5.cum+dat.tmp$GDD5[i] 
    }
    dat.tmp[i,"GDD5.cum"] <- gdd5.cum
  }
  met.all[met.all$YEAR==YR, "GDD5.cum"] <- dat.tmp$GDD5.cum
}
summary(met.all)

#This is the loop where we take our weather data and add it to our observation data frame
#So here we need to add in our MODIS data and change this code to fit the values in that data frame

dat.npn$GDD5.cum <- NA

for(DAT in paste(dat.npn$Date)){
  if(length(met.all[met.all$Date==as.Date(DAT), "GDD5.cum"]) > 0){
    dat.npn[dat.npn$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$Date==as.Date(DAT), "GDD5.cum"]
  }
}
View(dat.npn)


#-------------------------------------------------------------------------------------#
#This works for a single point at the arboretum using data we already have downloaded.
#However we are going to want to do this for locations all across the country.
#To do that we are going to need to use the daymetr package to download daymet data
#-------------------------------------------------------------------------------------#
#install.packages("daymetr")


#Setting the points to download the daymet data from
path.doc <- "C:/Users/lucie/Documents/NPN_data/"

#ystart <- min(dat.MODIS$Year)
ystart <- 2018

#yend <- max(dat.MODIS$Year)
yend <- 2019

pointsfile <- "MODIS_points.csv"

#Subsetting to only include lat and long (and for now the first rows to make testing easier)
#q.lat <- dat.npn[,(c=1:2)]
lat.in=41.812739
lon.in=-88.072749
q.lat <- data.frame("site" = "Daymet", "lat" = lat.in, "long" = lon.in)

#creating a proxy "site" column because the batch function needs it
#q.lat$site <- "Daymet"
#q.lat <- q.lat[,c(3,1,2)]
#q.lat <- unique(q.lat)

#Writing the csv file of lat and longs because batch function needs to read a file instead of a dataframe
write.csv(q.lat, file.path(path.doc, file = pointsfile), row.names=FALSE)


setwd(path.doc)
#Downloading all of the damet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = pointsfile,
                                           start = ystart,
                                           end = yend,
                                           internal = T)

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
                     longitude=rep(lat.list[[1]]$longitude, 365 * ((yend-ystart)+1)))

#Making sure we only go through relevant years we are calculating gdd5 for
dat.npn <- dat.npn[dat.npn$Year >= ystart, ]

#Looping to pull out the GDD5.cum of the bud burst date fore every tree and location
count <- 1
i <- 1
YR <- ystart
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
    
  }
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


#This script will serve to download the daymet weather data for every location we use for a set of years
#Setting the points to download the daymet data from
path.daymet <- '../data_raw/DAYMET'
if(!dir.exists(path.daymet)) dir.create(path.daymet)


# Note: We will probably want this to go by species, rather than site, but for now this works
site.id = 'MortonArb'

# This will probably get changed to a list of sites that we have NPN data for for each species
dat.MODIS <- read.csv(file.path('../data_raw/MODIS', paste0("MODIS_Greenup_", site.id, ".csv")))
summary(dat.MODIS)

#-------------------------------------------------------------------------------------#
#This works for a single point at the arboretum using data we already have downloaded.
#However we are going to want to do this for locations all across the country.
#To do that we are going to need to use the daymetr package to download daymet data
#-------------------------------------------------------------------------------------#
#install.packages("daymetr")


summary(dat.MODIS)


# Creating a point list and time range that matches your MODIS dataset
# Note: This will probably change down the road
modis.pts <- aggregate(greenup.year~site+latitude+longitude, data=dat.MODIS, 
                       FUN=min)
names(modis.pts)[4] <- "yr.start"
modis.pts$yr.end <- aggregate(greenup.year~site+latitude+longitude, data=dat.MODIS, 
                              FUN=max)[,4]
modis.pts


#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(modis.pts, file.path(path.daymet, paste0("TEST_POINTS_", site.id, ".csv")), row.names=FALSE)

# if(!dir.exist(path.daymet)) dir.create(path.daymet)
#Downloading all of the daymet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.daymet, paste0("TEST_POINTS_", site.id, ".csv")),
                                           start = min(modis.pts$yr.start),
                                           end = max(modis.pts$yr.end),
                                           internal = T)

# This gives us a list with one layer per site (I think)
length(lat.list)
names(lat.list) <- modis.pts$site # Giving the different layers of the list the site names they correspond to
# class(lat.list[[1]])
summary(lat.list[[1]])
summary(lat.list[[1]][["data"]]) # Lets us look at the data for the first site

#Lets look at the structure of what we are given
summary(lat.list)

#As you might notice we have a "List of 1" containing a nested "List of 7"
#Within that "List of 7" there is a data frame called data that has the data we want
#Well we know where it is now but how do we access it?

#This is the general format
lat.list[[1]]$latitude

#If you are reaching further into the data
# lat.list[[1]]$data$yday

# Creating a new simplified list that won't make Christy cranky
list.met <- list()
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=modis.pts$site[i], latitude=modis.pts$latitude[i], longitude=modis.pts$longitude[i], lat.list[[i]]$data)
}
names(list.met) <-  modis.pts$site
summary(list.met)
summary(list.met[[1]])

rm(lat.list) # Removing lat.list to save memory

#-----------------------------------------------------------------------#
#This is where we start preparing for the loop for GDD5 calculation.
# We'll take the daymet data, calculate GDD5 and make it a flattened object
#-----------------------------------------------------------------------#
#This package is needed for the %>% pipe used in the loop to summarize data
# library(dplyr)

#Making sure we only go through relevant years we are calculating gdd5 for

calc.gdd5 <- function(df.met){
  df.met$Date <- as.Date(paste(df.met$year, df.met$yday, sep="-"), format="%Y-%j")
  df.met$TMEAN <- (df.met$tmax..deg.c. + df.met$tmin..deg.c.)/2
  df.met$GDD5 <- ifelse(df.met$TMEAN>5, df.met$TMEAN-5, 0)
  df.met$GDD5.cum <- NA
  
  
  for(YR in min(df.met$year):max(df.met$year)){
    #df.yr is all weather data for a year at a location
    df.yr <- df.met[df.met$year==YR,]
    
    # Only calculate GDD5 if we have jan 1; this is Daymet, so it should be fine
    if(min(df.yr$yday)==1){
      # If we have Jan 1, calculate cumulative growing degree-days
      df.yr$GDD5.cum <- cumsum(df.yr$GDD5)
    } else {
      # if we're missing Jan 1: still create the column, but don't fill it
      df.yr$GDD5.cum <- NA 
    }
    
    # Note: we could have done this differently,but :shrug: this is easier to diagnose
    df.met[df.met$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
  } # end year loop
  
  return(df.met)
} # End funciton


# Apply our df.met function to all layers of our list;
# Note: we are overwriting the list, so be careful
list.met <- lapply(list.met, calc.gdd5)
summary(list.met[[1]])

# Unlist the met to save it to a dataframe that will be easier to share
df.met <- dplyr::bind_rows(list.met)
head(df.met)

# Quick graph to make sure things look okay
library(ggplot2)
ggplot(data=df.met) +
  geom_line(aes(x=yday, y=GDD5.cum, group=year))


write.csv(df.met, file.path(path.daymet, paste0("TEST_DAYMET_", site.id, ".csv")), row.names=FALSE)


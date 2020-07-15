#This script will serve to download the daymet weather data for every location we use for a set of years
#Setting the points to download the daymet data from

# Note: We will probably want this to go by species, rather than site, but for now this works
site.id = 'MortonArb'

# This will probably get changed to a list of sites that we have NPN data for for each species
dat.MODIS <- read.csv(file.path('../data_raw/MODIS', paste0("MODIS_Greenup_Raw_", site.id, ".csv")))
summary(dat.MODIS)

# Creating a point list and time range that matches your MODIS dataset
modis.pts <- aggregate(greenup.year~site+latitude+longitude, data=dat.MODIS,
FUN=min)
names(modis.pts)[4] <- "yr.start"
modis.pts$yr.end <- aggregate(greenup.year~site+latitude+longitude, data=dat.MODIS,
FUN=max)[,4]

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(modis.pts, file.path(path.DAYMET, paste0("MODIS_Coords_Raw_", site.id, ".csv")), row.names=FALSE)

#install.packages("daymetr")
#Downloading all of the daymet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.DAYMET, paste0("MODIS_Coords_Raw_", site.id, ".csv")),
                                           start = 2000,
                                           end = 2019,
                                           internal = T)

# This should give us a list with one layer per site
length(lat.list)
#names(lat.list) <- modis.pts$site # Giving the different layers of the list the site names they correspond to
# class(lat.list[[1]])
summary(lat.list[[1]])
summary(lat.list[[1]][["data"]])
summary(lat.list)

#Creating a simplified list of the information we want
list.met <- list()
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=modis.pts$site[i], latitude=modis.pts$latitude[i], longitude=modis.pts$longitude[i], lat.list[[i]]$data)
}
#names(list.met) <-  modis.pts$site
summary(list.met)
summary(list.met[[1]])

rm(lat.list) # Removing lat.list to save memory

#-----------------------------------------------------------------------#
#take the daymet data, calculate GDD5 and make it a flattened object
#-----------------------------------------------------------------------#
#This package is needed for the %>% pipe used in the loop to summarize data
# library(dplyr)

#Making sure we only go through relevant years we are calculating GDD5 for

calc.gdd5 <- function(df.met){
  df.met$Date <- as.Date(paste(df.met$year, df.met$yday, sep="-"), format="%Y-%j")
  df.met$TMEAN <- (df.met$tmax..deg.c. + df.met$tmin..deg.c.)/2
  df.met$GDD5 <- ifelse(df.met$TMEAN>5, df.met$TMEAN-5, 0)
  df.met$GDD5.cum <- NA
  
  
  for(YR in min(df.met$year):max(df.met$year)){
    #df.yr is all weather data for a year at a location
    df.yr <- df.met[df.met$year==YR,]
    
    # Only calculate GDD5 if we have Jan 1; this is Daymet, so it should be fine
    if(min(df.yr$yday)==1){
      # If we have Jan 1, calculate cumulative growing degree-days
      df.yr$GDD5.cum <- cumsum(df.yr$GDD5)
    } else {
      # if we're missing Jan 1: still create the column, but don't fill it
      df.yr$GDD5.cum <- NA 
    }
    
    # Note: we could have done this differently,but this should be easier to diagnose
    df.met[df.met$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
  } # end year loop
  
  return(df.met)
}

# Apply our df.met function to all layers of our list;
list.met <- lapply(list.met, calc.gdd5)
summary(list.met[[1]])

# Unlist the met to save it to a dataframe that will be easier to share
df.met <- dplyr::bind_rows(list.met)
head(df.met)

# Quick graph to make sure things look okay
library(ggplot2)
ggplot(data=df.met) +
  geom_line(aes(x=yday, y=GDD5.cum, group=year))


write.csv(df.met, file.path(path.DAYMET, paste0("DAYMET_Data_Raw_", site.id, ".csv")), row.names=FALSE)


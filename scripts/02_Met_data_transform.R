#This script will serve to download the daymet weather data for every location we use for a set of years
#Setting the points to download the daymet data from

# Note: We will probably want this to go by species, rather than site, but for now this works
species.name = 'Q.alba'

path.NPN <- "../data_raw/NPN/uncleaned"
if(!dir.exists(path.NPN)) dir.create(path.NPN)
oak.leaf <- read.csv(file.path(path.NPN, paste0("NPN_Quercus_Raw_", species.name, ".csv")))
summary(oak.leaf)
dim(oak.leaf)

# Creating a point list and time range that matches your MODIS dataset
NPN.pts <- aggregate(first_yes_year~site_id+latitude+longitude, data=oak.leaf,
FUN=min)
dim(NPN.pts)
names(NPN.pts)[4] <- "yr.start"
NPN.pts$yr.end <- aggregate(first_yes_year~site_id+latitude+longitude, data=oak.leaf,
FUN=max)[,4]
summary(NPN.pts)

NPN.pts$longitude[NPN.pts$longitude>0] <- NPN.pts$longitude[NPN.pts$longitude>0]*-1

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(NPN.pts, file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")), row.names=FALSE)
summary(NPN.pts)
#install.packages("daymetr")
#Downloading all of the daymet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")),
                                           start = 2000,
                                           end = 2019,
                                           internal = T)
names(lat.list) <- NPN.pts$site_id # Giving the different layers of the list the site names they correspond to

lat.list <- lat.list[sapply(lat.list, function(x) is.list(x))]

# This should give us a list with one layer per site
length(lat.list)

class(lat.list[[1]])
summary(lat.list[[1]])
summary(lat.list[[1]][["data"]])
summary(lat.list)

#Creating a simplified list of the information we want
# NPN.pts <- NPN.pts[c(1:256),]
list.met <- list()
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=NPN.pts$site_id[i], latitude=NPN.pts$latitude[i], longitude=NPN.pts$longitude[i], lat.list[[i]]$data)
}
names(list.met) <-  NPN.pts$site_id[1:length(list.met)]
summary(list.met)
summary(list.met[[1]])

rm(lat.list) # Removing lat.list to save memory

#-----------------------------------------------------------------------#
#take the daymet data, calculate GDD5 and make it a flattened object
#-----------------------------------------------------------------------#
#This package is needed for the %>% pipe used in the loop to summarize data
# library(dplyr)

#Making sure we only go through relevant years we are calculating GDD5 for
summary(NPN.pts)
NPN.pts <- list.met[[1]]
calc.gdd5 <- function(NPN.pts){
  NPN.pts$Date <- as.Date(paste(NPN.pts$year, NPN.pts$yday, sep="-"), format="%Y-%j")
  NPN.pts$TMEAN <- (NPN.pts$tmax..deg.c. + NPN.pts$tmin..deg.c.)/2
  NPN.pts$GDD5 <- ifelse(NPN.pts$TMEAN>5, NPN.pts$TMEAN-5, 0)
  NPN.pts$GDD5.cum <- NA
  
  
  for(YR in min(NPN.pts$year):max(NPN.pts$year)){
    #df.yr is all weather data for a year at a location
    df.yr <- NPN.pts[NPN.pts$year==YR,]
    
    # Only calculate GDD5 if we have Jan 1; this is Daymet, so it should be fine
    if(min(df.yr$yday)==1){
      # If we have Jan 1, calculate cumulative growing degree-days
      df.yr$GDD5.cum <- cumsum(df.yr$GDD5)
    } else {
      # if we're missing Jan 1: still create the column, but don't fill it
      df.yr$GDD5.cum <- NA 
    }
    
    # Note: we could have done this differently,but this should be easier to diagnose
    NPN.pts[NPN.pts$year==YR, "GDD5.cum"] <- df.yr$GDD5.cum
  } # end year loop
  
  return(NPN.pts)
}


# Apply our NPN.pts function to all layers of our list;
list.met <- lapply(list.met, calc.gdd5)
summary(list.met[[1]])

# Unlist the met to save it to a dataframe that will be easier to share
NPN.pts <- dplyr::bind_rows(list.met)
head(NPN.pts)

met.processed <- '../data_raw/DAYMET'
if(!dir.exists(met.processed)) dir.create(met.processed)
write.csv(NPN.pts, file.path(met.processed, paste0("DAYMET_Data_Processed_", species.name, ".csv")), row.names=FALSE)


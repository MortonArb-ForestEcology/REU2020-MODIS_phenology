#This script will serve to download the daymet weather data for every location we use for a set of years
#Setting the points to download the daymet data from

# Note: We will probably want this to go by species, rather than site, but for now this works
species.name = 'Q.alba'

path.NPN <- "../data_raw/NPN/uncleaned"
if(!dir.exists(path.NPN)) dir.create(path.NPN)
oak.leaf <- read.csv(file.path(path.NPN, paste0("NPN_Quercus_Raw_", species.name, ".csv")))
summary(oak.leaf)

#create a new column 'Year'. Going from 'xx-xx-2019' to '2019'
#new column HERE

# Creating a point list and time range that matches your MODIS dataset
NPN.pts <- aggregate(first_yes_year~site_id+latitude+longitude, data=oak.leaf,
FUN=min)
#what is this doing?
names(NPN.pts)[4] <- "yr.start"
NPN.pts$yr.end <- aggregate(first_yes_year~site_id+latitude+longitude, data=oak.leaf,
FUN=max)[,4]

path.DAYMET <- '../data_raw/DAYMET'
if(!dir.exists(path.DAYMET)) dir.create(path.DAYMET)
#Writing the csv file of lat and longs because daymetr batch function needs to read a file instead of a dataframe
write.csv(NPN.pts, file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")), row.names=FALSE)

#install.packages("daymetr")
#Downloading all of the daymet data for each point. Internal =TRUE means it creates a nested list. Set false to actually download a file
lat.list <- daymetr::download_daymet_batch(file_location = file.path(path.DAYMET, paste0("NPN_Coords_Raw_", species.name, ".csv")),
                                           start = 2000,
                                           end = 2019,
                                           internal = T)
is.recursive(list.met)
is.atomic(lat.list)
# This should give us a list with one layer per site
length(lat.list)
names(lat.list) <- NPN.pts$site_id # Giving the different layers of the list the site names they correspond to
class(lat.list[[1]])
summary(lat.list[[1]])
summary(lat.list[[1]][["data"]])
summary(lat.list)

#Creating a simplified list of the information we want
list.met <- list(NPN.pts)
for(i in seq_along(lat.list)){
  list.met[[i]] <- data.frame(site=NPN.pts$site[i], latitude=NPN.pts$latitude[i], longitude=NPN.pts$longitude[i], lat.list[[i]]['data'])
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

calc.gdd5 <- function(df.met){
  df.met$Date <- as.Date(paste(df.met$year, df.met$yday, sep="-"), format="%Y-%j")
  df.met$TMEAN <- (df.met$tmax..deg.c. + df.met$tmin..deg.c.)/2
  df.met$GDD5 <- ifelse(df.met$TMEAN>5, df.met$TMEAN-5, 0)
  df.met$GDD5.cum <- NA
  
  for(ST in min(list.met[site_id]):max(list.met[site_id])){
    #df.yr is all weather data for a year at a location
    met.site <- list.met[list.met$site_id==ST,]
    
    # Only calculate GDD5 if we have Jan 1; this is Daymet, so it should be fine
    if(min(met.site[met.site$first_yes_doy==1])){
      # If we have Jan 1, calculate cumulative growing degree-days
      met.site$GDD5.cum <- cumsum(met.site$GDD5)
    } else {
      # if we're missing Jan 1: still create the column, but don't fill it
      met.site$GDD5.cum <- NA 
    }
    
    # Note: we could have done this differently, but this should be easier to diagnose
    NPN.pts[NPN.pts$site_id==ST, "GDD5.cum"] <- NPN.pts$GDD5.cum
  } # end site loop
  
  return(NPN.pts)
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


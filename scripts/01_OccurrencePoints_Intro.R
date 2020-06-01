# ----------------------------
# Quick intro to working with occurrence points
# ----------------------------
# 0. Load libraries we'll want to use
# 1. Read in a file of point locations
# 2. Make a quick graph to make sure it looks right
#
# To be added
# 3. Do some data cleaning
# 4. Extract data associated with these points (e.g. MODIS data)
# ----------------------------



# ----------------------------
# 0. Load libraries we'll want to use
# ----------------------------
library(ggplot2); library(maps) # note: using a semicolon (;) will treat the commands as if they were on separate lines
# ----------------------------

# ----------------------------
# 1. Read in a file of point locations
# ----------------------------
spp.test <- read.csv("../data_raw/occurrence_points/Quercus_georgiana.csv", stringsAsFactors = T) # If you opened this file by double-clicking it, you should be in [something]/REU2020-MODIS_phenology/scripts.  The .. part of the file paths gets rid of the "/scripts" part and then from there, there should eb a folder called data_raw, etc.

summary(spp.test) # Look at the spp.test -- there's a lot there... what columns do you think would be useful?  Do values for things like "year" or latitude/longitude make sense?
# ----------------------------


# ----------------------------
# 2. Make a quick graph to make sure it looks right
# ----------------------------
# For spatial data, always start by plotting the points to see if they make sense.
map.world <- map_data("world") # Get the data associated with making a map of the world

ggplot() + # a statement that sets up a ggplot figure
  coord_equal() + # Sets the x & y scales so that the area given to 1 unit X is the same as 1 unit y (important for mapping -- see what happens if you comment this out!)
  ggtitle(unique(spp.test$taxon_name)) + # Adds a quick title based on our dataset
  geom_path(data=map.world, aes(x=long, y=lat, group=group)) + # Adds the country borders
  geom_point(data=spp.test, aes(x=decimalLongitude, y=decimalLatitude), color="red", size=2) + # adds dot for each occurrence point
  scale_x_continuous(expand=c(0,0)) + # doesn't add extra space to the x-axis
  scale_y_continuous(expand=c(0,0)) + # doesn't add extra space to the y-axis
  theme_minimal() # some more formatting


# A simpler graphing example; 
# What's going on with each part here?
ggplot(data=spp.test) +
  geom_bar(aes(x=database, fill=database), stat="count") +
  theme_minimal()


# Challenge: Make another type of graph summarizing something from this data
# Suggestions: Summarize some of the categorical data by making more bar plots or try a histogram or even adding color to the map! 
# Try some stuff and commit it to github!




# ----------------------------

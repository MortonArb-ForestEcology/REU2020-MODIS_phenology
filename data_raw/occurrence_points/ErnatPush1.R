# Christy has challenged us to make a change to the raw data she gave us on Github and push our changes.

setwd("REU2020-MODIS_phenology/data_raw/occurrence_points/")

REUPush1 = read.csv("Quercus_georgiana.csv")
  
dim(REUPush1)

summary(REUPush1)

names(REUPush1)

ggplot(data = REUPush1) +
  geom_line(mapping = aes(x = "year", y = "long_round", color = "green")) #I had a hard time finding a good looking graph to generate for this data, but it doesn't mattter for this.

            
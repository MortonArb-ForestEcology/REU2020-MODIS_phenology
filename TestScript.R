# Goal: This is a dummy test script to help orient with R.
# setwd("../REU2020-MODIS_phenology/")

# dir.create("data")
# dir.create("data_processed")
# Reading in the test datasets
test <- read.csv("data_test/RAW_test_data.csv")

dim(test)
names(test)
summary(test)

test$Taxon <- as.factor(test$Taxon)
test$Vernacular <- as.factor(test$Vernacular)
test$TaxList <- NA # Create a new blank columm
test$TaxList <- paste(test$Taxon, test$Obs.List, sep="-") # Create a new column combining taxa and list
head(test)
summary(test)
dim(test)

write.csv(test, "data_processed/TEST_RAW_CLEANED.csv", row.names=F)

# RESPONSE ~ PREDICTOR; DEPENDENT ~ INDEPENDENT; Y ~ X; Y=mx+b

# Latitude ~ Taxon
plot(BgLatitude ~ Taxon, data=test) 
plot(BgLatitude ~ BgLongitude, data=test)

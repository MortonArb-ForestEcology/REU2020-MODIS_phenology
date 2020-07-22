# Cleaning the NPN Data -- what we need is 1 observation per tree per year.

species.name <- 'Q.alba'

path.NPN <- "../data_raw/NPN/uncleaned"

#bringing in the NPN data to be filtered and cleaned
oak.leaf <- read.csv(file.path(path.NPN, paste0('NPN_Quercus_Raw_', species.name, '.csv')))
oak.leaf$species <- as.factor(oak.leaf$species)
oak.leaf$species_id <- as.factor(oak.leaf$species_id)
oak.leaf$individual_id <- as.factor(oak.leaf$individual_id)
oak.leaf$phenophase_id <- as.factor(oak.leaf$phenophase_id)
oak.leaf$phenophase_description <- as.factor(oak.leaf$phenophase_description)
unique(oak.leaf$phenophase_id)
summary(oak.leaf$numdays_since_prior_no)

# ------------------------------------------
# Deciding what data is "good" or "bad"
# ------------------------------------------
# Using a 10-day thresholds since prior/next no as an indicator of questionable data  or if there is no value for that (first obs of the year)
# if the days since last no is greater than 10 OR (| mean OR) there is no "no" observation before a yes
summary(oak.leaf$first_yes_doy)
oak.leaf[oak.leaf$numdays_since_prior_no>10 | is.na(oak.leaf$numdays_since_prior_no), c("first_yes_doy", "first_yes_julian_date")] <- NA
summary(oak.leaf$first_yes_doy)

summary(oak.leaf$last_yes_doy)
oak.leaf[oak.leaf$numdays_until_next_no>10 | is.na(oak.leaf$numdays_until_next_no), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf$last_yes_doy)

# BLB = 371; FL= 471; Leaves = 483
########## --------------------- ################
# Bud burst
########## --------------------- ################
# Getting rid of bud burst after July 1 (~182) because we just want SPRING phenology
oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$first_yes_doy>182 & !is.na(oak.leaf$first_yes_doy), c("first_yes_doy", "first_yes_julian_date")] <- NA
oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$last_yes_doy>182 & !is.na(oak.leaf$last_yes_doy), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf[oak.leaf$phenophase_id==371,])

# So now we feel like we have relatively good budburst data, but we still have multiple observaitons per tree; options: earliest, latest, mean
# Aggregating using a formula; in R, y=mx+b is y ~ m*x + b 
dat.budburst <- data.frame(individual_id=rep(unique(oak.leaf$individual_id[oak.leaf$phenophase_id==371]), each=length(unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==371]))),
                           year=unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==371]))
summary(dat.budburst)                           
dim(dat.budburst)
dim(oak.leaf)
#creating summary columns from the first and last parts of 'Breaking Leaf Buds'
for(IND in unique(dat.budburst$individual_id)){
  # adding some individual metadata -- this only needs to be done for each tree; we don't care about which year it is
  dat.budburst[dat.budburst$individual_id==IND, c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")] <- unique(oak.leaf[oak.leaf$individual_id==IND,c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")])
  
  for(YR in unique(dat.budburst$year[dat.budburst$individual_id==IND])){
    # creating a handy index for what row we're working with
       
    row.now <- which(dat.budburst$individual_id==IND & dat.budburst$year==YR)
    
    # Just narrowing the data frame down to just the part we want to work with
    dat.tmp <- oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$individual_id==IND & oak.leaf$first_yes_year==YR,]
    
    if(nrow(dat.tmp)==0) next # skips through if there's no data
    
    if(nrow(dat.tmp)==1){
      dat.budburst[row.now, c("first.mean", "first.min", "first.max")] <- dat.tmp$first_yes_doy
      dat.budburst[row.now, c("last.mean", "last.min", "last.max")] <- dat.tmp$last_yes_doy
    } else {
      dat.budburst[row.now, "first.mean"] <- mean(dat.tmp$first_yes_doy, na.rm=T)
      dat.budburst[row.now, "first.min" ] <- min(dat.tmp$first_yes_doy, na.rm=T)
      dat.budburst[row.now, "first.max" ] <- max(dat.tmp$first_yes_doy, na.rm=T)
      
      dat.budburst[row.now, "last.mean"] <- mean(dat.tmp$last_yes_doy, na.rm=T)
      dat.budburst[row.now, "last.min" ] <- min(dat.tmp$last_yes_doy, na.rm=T)
      dat.budburst[row.now, "last.max" ] <- max(dat.tmp$last_yes_doy, na.rm=T)
    }
  }
}

summary(dat.budburst)

#Checking to see how the data changed from the loop
dim(dat.budburst); dim(oak.leaf[oak.leaf$phenophase_id==371,])

#removes the Inf/-Inf values
dat.budburst[dat.budburst$first.min== 'Inf' & !is.na(dat.budburst$first.min), 'first.min'] <- NA
dat.budburst[dat.budburst$first.min== '-Inf' & !is.na(dat.budburst$first.min), 'first.min'] <- NA

dat.budburst <- dat.budburst[!is.na(dat.budburst$first.min),]

#visual for the cleaned NPN data, focusing on the minimum value of the first observed yes DOY in 'Breaking Leaf Buds'
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('Clean_firstmin_bud_', species.name, '_NPN.png')))

hist(dat.budburst$first.min)

dev.off()

summary(dat.budburst)

########## --------------------- ################
# Leaves '483'
########## --------------------- ################

#insufficient evidence to constrain the leaves observations by day of year. 
oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$first_yes_doy>182 & !is.na(oak.leaf$first_yes_doy), c("first_yes_doy", "first_yes_julian_date")] <- NA
oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$last_yes_doy>182 & !is.na(oak.leaf$last_yes_doy), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf[oak.leaf$phenophase_id==483,])
dim(oak.leaf[oak.leaf$phenophase_id==483,])

# So now we feel like we have relatively good "leaves" data, but we still have multiple observations per tree; options: earliest, latest, mean
# We're making the data APPROPRIATE for the hypothesis rather than making it MEET the hypothesis

# Aggregating using a formula; in R, y=mx+b is y ~ m*x + b 
dat.leaves <- data.frame(individual_id=rep(unique(oak.leaf$individual_id[oak.leaf$phenophase_id==483]), each=length(unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==483]))),
                           year=unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==483]))
summary(dat.leaves)                           

#creating summary columns from the first and last parts of 'Breaking Leaf Buds'
for(IND in unique(dat.leaves$individual_id)){
  # adding some individual metadata -- this only needs to be done for each tree; we don't care about which year it is
  dat.leaves[dat.leaves$individual_id==IND, c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")] <- unique(oak.leaf[oak.leaf$individual_id==IND,c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")])
  
  for(YR in unique(dat.leaves$year[dat.leaves$individual_id==IND])){
    # creating a handy index for what row we're working with
    row.now <- which(dat.leaves$individual_id==IND & dat.leaves$year==YR)
    
    # Just narrowing the data frame down to just the part we want to work with
    dat.tmp <- oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$individual_id==IND & oak.leaf$first_yes_year==YR,]
    
    if(nrow(dat.tmp)==0) next # skips through if there's no data
    
    if(nrow(dat.tmp)==1){
      dat.leaves[row.now, c("first.mean", "first.min", "first.max")] <- dat.tmp$first_yes_doy
      dat.leaves[row.now, c("last.mean", "last.min", "last.max")] <- dat.tmp$last_yes_doy
    } else {
      dat.leaves[row.now, "first.mean"] <- mean(dat.tmp$first_yes_doy, na.rm=T)
      dat.leaves[row.now, "first.min" ] <- min(dat.tmp$first_yes_doy, na.rm=T)
      dat.leaves[row.now, "first.max" ] <- max(dat.tmp$first_yes_doy, na.rm=T)
      
      dat.leaves[row.now, "last.mean"] <- mean(dat.tmp$last_yes_doy, na.rm=T)
      dat.leaves[row.now, "last.min" ] <- min(dat.tmp$last_yes_doy, na.rm=T)
      dat.leaves[row.now, "last.max" ] <- max(dat.tmp$last_yes_doy, na.rm=T)
    }
  }
}
#checking to see if it got rid of the multiple entries per year for a single tree.
summary(dat.leaves)
dim(dat.leaves); dim(oak.leaf[oak.leaf$phenophase_id==483,])

#removes the Inf/-Inf values
dat.leaves[dat.leaves$first.min== 'Inf' & !is.na(dat.leaves$first.min), 'first.min'] <- NA
dat.leaves[dat.leaves$first.min== '-Inf' & !is.na(dat.leaves$first.min), 'first.min'] <- NA

dat.leaves <- dat.leaves[!is.na(dat.leaves$first.min),]

#visual for the cleaned NPN data, focusing on the minimum value of the first observed yes DOY in 'Leaves'
path.png <- '../figures/'
if(!dir.exists(path.png)) dir.create(path.png, recursive=T)
png(filename= file.path(path.png, paste0('Clean_firstmin_leaf_', species.name, '_NPN.png')))

hist(dat.leaves$first.min)

dev.off()

summary(dat.leaves)

#saving the cleaned NPN data that has been split by phenophase
path.clean <- "../data_raw/NPN/cleaned"
if(!dir.exists(path.clean)) dir.create(path.clean)
write.csv(dat.budburst, file.path(path.clean, paste0('NPN_Quercus_bud_', species.name, '.csv')), row.names=F)
write.csv(dat.leaves, file.path(path.clean, paste0('NPN_Quercus_leaf_', species.name, '.csv')), row.names=F)


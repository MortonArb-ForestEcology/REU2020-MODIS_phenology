# Cleaning the NPN Data -- what we need is 1 observation per tree per year.  We don't have that quite yet.

library(ggplot2)
site.id <- 'MortonArb'

path.npn <- "../data_raw/NPN/"

oak.leaf <- read.csv(file.path(path.npn, paste0('Quercus_', site.id, '.csv')))
oak.leaf$species <- as.factor(oak.leaf$species)
oak.leaf$species_id <- as.factor(oak.leaf$species_id)
oak.leaf$individual_id <- as.factor(oak.leaf$individual_id)
oak.leaf$phenophase_id <- as.factor(oak.leaf$phenophase_id)
oak.leaf$phenophase_description <- as.factor(oak.leaf$phenophase_description)
oak.leaf$site_id <- site.id
summary(oak.leaf)

# ------------------------------------------
# Deciding what data is "good" or "bad"
# ------------------------------------------
# Using a 10-day thresholds since prior/next no as an indicator of questionable data  or if there is no value for that (first obs of the year)
# What this code says data[ROWS, COLUMNS]
# if the days since last no is greather than 10 OR (| mean OR) there is no "no" observation before a yes
oak.leaf[oak.leaf$numdays_since_prior_no>10 | is.na(oak.leaf$numdays_since_prior_no), c("first_yes_doy", "first_yes_julian_date")] <- NA

oak.leaf[oak.leaf$numdays_until_next_no>10 | is.na(oak.leaf$numdays_until_next_no), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf)

# looking at some of the weird events
# BLB = 371; FL= 471; Leaves = 483


########## --------------------- ################
# Bud burst
########## --------------------- ################
hist(oak.leaf[oak.leaf$phenophase_id==371, "last_yes_doy"])

# Getting rid of bud burst after July 1 (~182) because we just want SPRING budburst
oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$first_yes_doy>182 & !is.na(oak.leaf$first_yes_doy), c("first_yes_doy", "first_yes_julian_date")] <- NA
oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$last_yes_doy>182 & !is.na(oak.leaf$last_yes_doy), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf[oak.leaf$phenophase_id==371,])
10/95

# So now we feel like we have relatively good budburst data, but we still have multiple observaitons per tree; options: earliest, latest, mean
# We're making the data APPROPRIATE for the hypothesis rather than making it MEET the hypothesis
oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$individual_id==132863,]
# Aggregateing using a formula; in R, y=mx+b is y ~ m*x + b 
dat.budburst <- data.frame(individual_id=rep(unique(oak.leaf$individual_id[oak.leaf$phenophase_id==371]), each=length(unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==371]))),
                           year=unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==371]))
summary(dat.budburst)                           

for(IND in unique(dat.budburst$individual_id)){
  # adding some individual metadata -- this only needs to be done for each tree; we dont' care about which year it is
  dat.budburst[dat.budburst$individual_id==IND, c("site.id", "latitude", "longitude", "species_id", "genus", "species", "common_name")] <- unique(oak.leaf[oak.leaf$individual_id==IND,c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")])
  
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
dim(dat.budburst); dim(oak.leaf[oak.leaf$phenophase_id==371,])
dat.budburst[dat.budburst$individual_id==132863,]

hist(dat.budburst$first.mean)
hist(dat.budburst$last.mean)



ggplot(data = dat.budburst, mapping = aes(x = species == c('alba', 'rubra'), y = first.mean)) +
  ggtitle('3 year averages for 4 Quercus species bud burst onset at The Morton Arboretum') +
  geom_line()

 


########## --------------------- ################


########## --------------------- ################
# Leaves '483'
########## --------------------- ################
hist(oak.leaf[oak.leaf$phenophase_id==483, 'first_yes_doy'])
hist(oak.leaf[oak.leaf$phenophase_id==483, 'last_yes_doy'])
summary(oak.leaf[oak.leaf$phenophase_id==483,])

# Getting rid of leaf phenophase observations after Sept 22 (first day of Autumn ~265) because we just want new/live leaves per the usanpn "Leaves" description for Tree/Shrub.
oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$first_yes_doy>365 & !is.na(oak.leaf$first_yes_doy), c("first_yes_doy", "first_yes_julian_date")] <- NA
oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$last_yes_doy>365 & !is.na(oak.leaf$last_yes_doy), c("last_yes_doy", "last_yes_julian_date")] <- NA
summary(oak.leaf[oak.leaf$phenophase_id==483,])
dim(oak.leaf[oak.leaf$phenophase_id==483,])

# So now we feel like we have relatively good "leaves" data, but we still have multiple observations per tree; options: earliest, latest, mean
# We're making the data APPROPRIATE for the hypothesis rather than making it MEET the hypothesis

#checking to ensure that the multiple observations issue stil persists in this phenophase
oak.leaf[oak.leaf$phenophase_id==483 & oak.leaf$individual_id==132863,]

# Aggregating using a formula; in R, y=mx+b is y ~ m*x + b 
dat.leaves <- data.frame(individual_id=rep(unique(oak.leaf$individual_id[oak.leaf$phenophase_id==483]), each=length(unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==483]))),
                           year=unique(oak.leaf$first_yes_year[oak.leaf$phenophase_id==483]))
summary(dat.leaves)                           

for(IND in unique(dat.leaves$individual_id)){
  # adding some individual metadata -- this only needs to be done for each tree; we don't care about which year it is
  dat.leaves[dat.leaves$individual_id==IND, c("site.id", "latitude", "longitude", "species_id", "genus", "species", "common_name")] <- unique(oak.leaf[oak.leaf$individual_id==IND,c("site_id", "latitude", "longitude", "species_id", "genus", "species", "common_name")])
  
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
dat.leaves[dat.leaves$individual_id==132863,]
hist(dat.leaves$first.mean)
hist(dat.leaves$last.mean)

#removes the Inf/-Inf values that I do not remember how they got there -Andrew
dat.leaves[dat.leaves$last.min== 'Inf' & !is.na(dat.leaves$last.min), 'last.min'] <- NA
dat.leaves[dat.leaves$last.max== '-Inf' & !is.na(dat.leaves$last.max), 'last.max'] <- NA


# Falling Leaves
summary(oak.leaf[oak.leaf$phenophase_id==471,])
# ------------------------------------------

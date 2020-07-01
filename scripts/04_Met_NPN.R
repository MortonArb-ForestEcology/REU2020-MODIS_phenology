

#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("TEST_DAYMET_", site.id, ".csv")))
summary(df.met)

#get NPN data
dat.alba <- read.csv(file.path('../data_raw/NPN', paste0("Alba_MortonArb_QUAL.csv")))
summary(dat.alba)

dat.macrocarpa <- read.csv(file.path('../data_raw/NPN', paste0("Bur_MortonArb_QUAL.csv")))
summary(dat.macrocarpa)
#----------------------------
head(dat.alba)
head(dat.macrocarpa)

dat.alba$phases.yday <- (dat.alba$first_yes_doy + dat.alba$last_yes_doy)/2 #get a mean of each phenophase yday
dat.macrocarpa$phases.yday <- (dat.macrocarpa$first_yes_doy + dat.macrocarpa$last_yes_doy)/2 #get a mean of each phenophase yday


dat.alba$phases.yday <- round(x= dat.alba$phases.yday, digits = 0)
summary(dat.alba$phases.yday)

dat.macrocarpa$phases.yday <- round(x= dat.macrocarpa$phases.yday, digits = 0)
summary(dat.macrocarpa$phases.yday)

unique(dat.alba$individual_id)
unique(dat.macrocarpa$individual_id)


for(i in 1:nrow(dat.alba)){
  # We need to use med.bud
  bud.yday <- dat.alba[i, 'phases.yday']
  bud.year <- dat.alba[i, "first_yes_year"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.alba[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.yday, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}

for(i in 1:nrow(dat.macrocarpa)){
  # We need to use med.bud
  bud.yday <- dat.macrocarpa[i, 'phases.yday']
  bud.year <- dat.macrocarpa[i, "first_yes_year"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.macrocarpa[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.yday, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}

dat.alba$GDD5.cum <- round(x= dat.alba$GDD5.cum, digits = 0)
head(dat.alba)

dat.macrocarpa$GDD5.cum <- round(x= dat.macrocarpa$GDD5.cum, digits = 0)
head(dat.macrocarpa)

alba.burst <- dat.alba[dat.alba$phenophase_id == '371',]
summary(alba.burst)
alba.burst$burst.yday <- alba.burst$phases.yday

bur.burst <- dat.macrocarpa[dat.macrocarpa$phenophase_id == '371',]
summary(bur.burst)
bur.burst$burst.yday <- bur.burst$phases.yday

library(ggplot2)
ggplot(data = alba.burst, mapping = aes(x= GDD5.cum, y= burst.yday)) + 
  facet_wrap(alba.burst$first_yes_year) +
  geom_point()

ggplot(data = bur.burst, mapping = aes(x= GDD5.cum, y= burst.yday)) + 
  facet_wrap(bur.burst$first_yes_year) +
  geom_point()

processed.npn <- '../data_processed/'
if(!dir.exists(processed.npn)) dir.create(processed.npn)
write.csv(dat.alba, file.path(processed.npn, "Alba_MortonArb_NPN_MET.csv"), row.names=F)
write.csv(dat.macrocarpa, file.path(processed.npn, "Bur_MortonArb_NPN_MET.csv"), row.names=F)


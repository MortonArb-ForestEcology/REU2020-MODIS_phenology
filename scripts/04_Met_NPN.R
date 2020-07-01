

#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("TEST_DAYMET_", site.id, ".csv")))
summary(df.met)

#get NPN data
dat.npn <- read.csv(file.path('../data_raw/NPN', paste0("TEST_MortonArb_QUAL.csv")))
summary(dat.npn)
#----------------------------
head(dat.npn)

dat.npn$phases.yday <- (dat.npn$first_yes_doy + dat.npn$last_yes_doy)/2 #get a mean of each phenophase yday


dat.npn$phases.yday <- round(x= dat.npn$phases.yday, digits = 0)
summary(dat.npn$phases.yday)

unique(dat.npn$individual_id)


for(i in 1:nrow(dat.npn)){
  # We need to use med.bud
  bud.yday <- dat.npn[i, 'phases.yday']
  bud.year <- dat.npn[i, "first_yes_year"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.npn[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.yday, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}

dat.npn$GDD5.cum <- round(x= dat.npn$GDD5.cum, digits = 0)
head(dat.npn)

dat.burst <- dat.npn[dat.npn$phenophase_id == '371',]
summary(dat.burst)
dat.burst$burst.yday <- dat.burst$phases.yday

library(ggplot2)
ggplot(data = dat.burst, mapping = aes(x= GDD5.cum, y= burst.yday)) + 
  facet_wrap(dat.burst$first_yes_year) +
  geom_point()

processed.npn <- '../data_processed/NPN'
if(!dir.exists(processed.npn)) dir.create(processed.npn)
write.csv(dat.burst, file.path(processed.npn, "TEST_MortonArb_NPN_MET.csv"), row.names=F)




#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("TEST_DAYMET_", site.id, ".csv")))
summary(df.met)

#get NPN data
dat.npn <- read.csv(file.path('../data_raw/NPN', paste0("TEST_MortonArb_QUAL.csv")))
summary(dat.npn)
#----------------------------
head(dat.npn)

dat.npn$mean.phase <- (dat.npn$first_yes_doy + dat.npn$last_yes_doy)/2 #get a mean of each phenophase yday

#need to not take the gdd5 of all phenophases, but only breaking leaf buds
yday.burst <- dat.npn[dat.npn$phenophase_id == '371',]

head(yday.burst)

dat.npn$mean.bud <- round(x= dat.npn$mean.bud, digits = 0)

unique(dat.npn$individual_id)

summary(dat.npn$mean.bud)

for(i in 1:33){
  # We need to use med.bud
  bud.yday <- dat.npn[i, 'yday.bud']
  bud.year <- dat.npn[i, "first_yes_year"]
  bud.m <- dat.npn[i, "mean.phase"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.npn[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.m, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}

dat.npn$GDD5.cum <- round(x= dat.npn$GDD5.cum, digits = 0)
head(dat.npn)

library(ggplot2)
ggplot(data = dat.npn, mapping = aes(x= GDD5.cum, y= mean.phase)) + 
  geom_point()

write.csv(dat.npn, file.path(path.npn, "TEST_MortonArb_NPN_MET.csv"), row.names=F)


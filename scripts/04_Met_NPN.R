

#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("TEST_DAYMET_", site.id, ".csv")))
summary(df.met)

#get NPN data
dat.npn <- read.csv(file.path('../data_raw/NPN', paste0("TEST_MortonArb_QUAL.csv")))
summary(dat.npn)
#----------------------------
head(dat.npn)

dat.npn$mean.bud <- (dat.npn$first_yes_doy + dat.npn$last_yes_doy)/2 #get a mean bud burst yday
#dat.burst <- dat.npn[dat.npn$phenophase_id == '371',]

head(dat.npn)
dat.npn$mean.bud <- round(x= dat.npn$mean.bud, digits = 0)

summary(dat.npn$mean.bud)

#the smaller frame needs to constrict the larger frame will only go to row 26 before having to input 0, = error
for(i in 1:nrow(df.met)){
  # We need to use med.bud
  bud.year <- dat.npn[i, "first_yes_year"]
  bud.m <- dat.npn[i, "mean.bud"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.npn[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.m, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}
View(dat.npn)

write.csv(dat.npn, file.path(path.npn, "TEST_MortonArb_NPN_MET.csv"), row.names=F)




#get DAYMET data
site.id <- 'MortonArb'
df.met <- read.csv(file.path('../data_raw/DAYMET', paste0("DAYMET_Data_", site.id, ".csv")))
summary(df.met)

#get NPN data
dat.macrocarpa <- read.csv(file.path('../data_raw/NPN', paste0("Bur_MortonArb_QUAL.csv")))
summary(dat.macrocarpa)

dat.arb <- read.csv("../data_raw/NPN/TEST_MortonArb_OakCollection_PhenoLeaf.csv")
#----------------------------

head(dat.macrocarpa)

dat.macrocarpa$phases.yday <- (dat.macrocarpa$first_yes_doy + dat.macrocarpa$last_yes_doy)/2 #get a mean of each phenophase yday

unique(dat.macrocarpa$individual_id)


for(i in 1:nrow(dat.macrocarpa)){
  # We need to use med.bud
  bud.yday <- dat.macrocarpa[i, 'phases.yday']
  bud.year <- dat.macrocarpa[i, "first_yes_year"]
  
  # We need to get certain rows --> we need 2 pieces of info to match
  #  we need BOTH year and yday to match that for the dat.npn row we're working with
  dat.macrocarpa[i,"GDD5.cum"] <- df.met[df.met$year==bud.year & df.met$yday==bud.yday, "GDD5.cum"] #df.met$year==bud.year & df.met$yday==bud.m,
}


bur.burst <- dat.macrocarpa[dat.macrocarpa$phenophase_id == '371',]
summary(bur.burst)
bur.burst$burst.yday <- bur.burst$phases.yday

library(ggplot2)

ggplot(data = bur.burst, mapping = aes(x= GDD5.cum, y= burst.yday)) + 
  facet_wrap(bur.burst$first_yes_year) +
  geom_point()

processed.npn <- '../data_processed/'
if(!dir.exists(processed.npn)) dir.create(processed.npn)
write.csv(bur.burst, file.path(processed.npn, "Bur_MortonArb_NPN_MET.csv"), row.names=F)


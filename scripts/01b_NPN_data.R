# Download Phenology data from NPN

# -----------------------------------
# NOTE: rnpn needs to be installed from GITHUB -- not CRAN!
#  i.e. you can NOT just do library('rnpn') -- you'll have an older version
# -----------------------------------
#install.packages("devtools")
#library('devtools')
#devtools::install_github("usa-npn/rnpn")

library(rnpn)
site.id <- 'MortonArb'

# -----------------------------------

# -----------------------------------
#npn_stations_with_spp
# Getting a list of all species
#npn.spp <- npn_species()
#npn.quercus <- npn.spp[npn.spp$genus=="Quercus",]

#quercus.phenophase <- npn_get_phenophases_for_taxon(genus_ids=unique(npn.quercus$genus_id))

# To get pre-summarized data, use npn_donwload_individual_phenometrics
# to get help: ?npn_download_individual_phenometrics 
# species_ids = what species you're interested in; just work with 1 at a time for now; can get rid of if you want everything from our Oak Collection
# station_ids=26202 gets just The Morton Arboretum Oak Collection (just for now)
# years = we can narrow this down, but we probably want everything we can get
# request_source = something the function needs so NPN knows whose using this
#dat.NPN <- npn_download_individual_phenometrics(species_ids=npn.quercus$species_id[npn.quercus$species=="macrocarpa"],
                                                 #station_ids = 26202, years=2000:2020
                                                 #, request_source="The Morton Arboretum")

#?npn_download_individual_phenometrics
#dat.NPN[dat.NPN==-9999] <- NA
#unique(dat.NPN$phenophase_description)
#summary(dat.NPN)

# Note: this has more phenophases that we actually want right now, but lets just stick with it
#write.csv(dat.NPN, file.path(path.NPN, paste0("NPN", site.id, "_QUAL.csv")), row.names=F)

# Doing another quick search for the Arb, but getting everything from our oak collection; note that it is MUCH slower
# We're also only get a couple phenophases that we actually care about for this project
#phenophase.leaves <- unique(dat.NPN$phenophase_id[dat.NPN$phenophase_description %in% c("Breaking leaf buds", "Leaves", "Falling leaves")])


oak.leaf <- npn_download_individual_phenometrics(phenophase_ids =c(371, 471, 483), station_ids = 26202, years=2000:2020, request_source="The Morton Arboretum")
oak.leaf[oak.leaf==-9999] <- NA
summary(oak.leaf)
dim(oak.leaf)

#Bud Burst and Leaves will be the two phenophases of most interest because we care about Spring phenology right now
oak.budburst <- oak.leaf[oak.leaf$phenophase_id == '371']
summary(oak.budburst)
oak.leaves <- oak.leaf[oak.leaf$phenophase_id == '471']
summary(oak.leaves)
oak.fallen <- oak.leaf[oak.leaf$phenophase_id == '483']
summary(oak.fallen)

library(ggplot2)
path.figures <- '../REU2020-MODIS_phenology/figures'
if(!dir.exists(path.figures)) dir.create(path.figures, recursive=T)
png(filename= file.path(path.figures, paste0('NPN_firstbudburst_uncleaned_', site.id, '.png')))

ggplot(data = oak.budburst) +
  geom_boxplot(mapping = aes(x = species, y = first_yes_doy)) +
  geom_point(mapping = aes(x = species, y = last_yes_doy, color = as.character(last_yes_year))) +
  scale_x_discrete(' Q. species') +
  scale_y_continuous('First Budburst Onset Observation (Yday)') +
  ggtitle('First Budburst Observation for Quercus in 2017-2019 from NPN at The Morton Arboretum')
dev.off()

length(unique(oak.leaf$species)) # 12 unique species
length(unique(oak.leaf$individual_id)) # 31 unique trees

path.NPN <- "../data_raw/NPN/uncleaned"
if(!dir.exists(path.NPN)) dir.create(path.NPN)
write.csv(oak.leaf, file.path(path.NPN, paste0('NPN_Quercus_Raw_', site.id, '.csv')), row.names=F)


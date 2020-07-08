# Download Phenology data from NPN
# NOTE: for right now, probably just start with the NPN individual phenometrics which will get us the first observation for each individual being observed
# If we have trouble with this, we'll probably want to play with the raw data to get some better cleaned data

# -----------------------------------
# NOTE: rnpn needs to be installed from GITHUB -- not CRAN!
#  i.e. you can NOT just do library('rnpn') -- you'll have an older version
# -----------------------------------
# install.packages("devtools")
# library('devtools')
# devtools::install_github("usa-npn/rnpn")
library(rnpn)
site.id <- 'MortonArb'

path.npn <- "../data_raw/NPN/"
if(!dir.exists(path.npn)) dir.create(path.npn)

# -----------------------------------


# -----------------------------------
# -----------------------------------
#npn_stations_with_spp
# Getting a list of all species
npn.spp <- npn_species()
npn.quercus <- npn.spp[npn.spp$genus=="Quercus",]

quercus.phenophase <- npn_get_phenophases_for_taxon(genus_ids=unique(npn.quercus$genus_id))

# To get pre-summarized data, use npn_donwload_individual_phenometrics
# to get help: ?npn_download_individual_phenometrics 
# species_ids = what species you're interested in; just work with 1 at a time for now; can get rid of if you want everything from our Oak Collection
# station_ids=26202 gets just The Morton Arboretum Oak Collection (just for now)
# years = we can narrow this down, but we probably want everything we can get
# request_source = something the function needs so NPN knows whose using this
dat.npn <- npn_download_individual_phenometrics(species_ids=npn.quercus$species_id[npn.quercus$species=="macrocarpa"],
                                                 station_ids = 26202, years=2000:2020
                                                 , request_source="The Morton Arboretum")

?npn_download_individual_phenometrics
dat.npn[dat.npn==-9999] <- NA
unique(dat.npn$phenophase_description)
summary(dat.npn)

# Note: this has more phenophases that we actually want right now, but lets just stick with it
write.csv(dat.npn, file.path(path.npn, paste0("NPN", site.id, "_QUAL.csv")), row.names=F)

# Doing another quick search for the Arb, but getting everything from our oak collection; note that it is MUCH slower
# We're also only get a couple phenophases that we actually care about for this project
phenophase.leaves <- unique(dat.npn$phenophase_id[dat.npn$phenophase_description %in% c("Breaking leaf buds", "Leaves", "Falling leaves")])
phenophase.leaves

oak.leaf <- npn_download_individual_phenometrics(phenophase_ids =c(371, 471, 483), station_ids = 26202, years=2000:2020, request_source="The Morton Arboretum")
oak.leaf[oak.leaf==-9999] <- NA
summary(oak.leaf)
dim(oak.leaf)

oak.budburst <- oak.leaf[oak.leaf$phenophase_id == '371']
#refer to https://docs.google.com/document/d/1hHlDuY8WzCpZHmai323gRfmsfiNX9BhK5QYN7l93XsQ/edit, step 3 
summary(oak.budburst[oak.budburst$species =='gambelii'], oak.budburst[oak.budburst$first_yes_year == '2017' ])

#trying to get rid of multiple consecutive budburst series within the same year for the same tree.
burst.id <- aggregate(oak.budburst, by = list(oak.budburst$individual_id), FUN = TMAX)



oak.leaves <- oak.leaf[oak.leaf$phenophase_id == '471']
oak.fallen <- oak.leaf[oak.leaf$phenophase_id == '483']

library(ggplot2)
#alter object to see different raw NPN data for the phenophases of interest.
ggplot(data = oak.budburst) +
  geom_boxplot(mapping = aes(x = species, y = last_yes_doy)) +
  geom_point(mapping = aes(x = species, y = last_yes_doy, color = as.character(last_yes_year))) +
  scale_x_discrete(' Q. species') +
  scale_y_continuous('Last Budburst (Yday)') +
  ggtitle('Last Budburst Event for Quercus in 2017-2019 from NPN at The Morton Arboretum')

  
length(unique(oak.leaf$species)) # 12 unique species
length(unique(oak.leaf$individual_id)) # 31 unique trees

write.csv(oak.leaf, file.path(path.npn, paste0('Quercus_', site.id, '.csv')), row.names=F)

# -----------------------------------




# Download Phenology data from NPN

# -----------------------------------
# NOTE: rnpn needs to be installed from GITHUB -- not CRAN!
#  i.e. you can NOT just do library('rnpn') -- you'll have an older version
# -----------------------------------
#install.packages("devtools")
#library('devtools')
#devtools::install_github("usa-npn/rnpn")

library(rnpn)
species.name <- 'Q.alba'

# -----------------------------------
# To get pre-summarized data, use npn_donwload_individual_phenometrics
# to get help: ?npn_download_individual_phenometrics 

npn.spp <- npn_species()
npn.quercus <- npn.spp[npn.spp$genus=="Quercus",]

oak.leaf <- npn_download_individual_phenometrics(phenophase_ids =c(371, 471),species_ids=npn.quercus$species_id[npn.quercus$species=="alba"], years=2000:2019, request_source="The Morton Arboretum")
oak.leaf[oak.leaf==-9999] <- NA
dim(oak.leaf)
head(oak.leaf)

#Bud Burst and Leaves will be the two phenophases of most interest because we care about Spring phenology right now
oak.budburst <- oak.leaf[oak.leaf$phenophase_id == '371']
summary(oak.budburst)
oak.leaves <- oak.leaf[oak.leaf$phenophase_id == '471']
summary(oak.leaves)

#260 individual trees for WHite Oak "Breaking Leaf Buds" 
length(unique(oak.budburst$individual_id))

#261 individual trees for White Oak "Leaves"
length(unique(oak.leaves$individual_id))

#taking a look at the trends of the first observations for leaves and bud burst
hist(oak.budburst$first_yes_doy)
hist(oak.leaves$first_yes_doy)


path.NPN <- "../data_raw/NPN/uncleaned"
if(!dir.exists(path.NPN)) dir.create(path.NPN)
write.csv(oak.leaf, file.path(path.NPN, paste0('NPN_Quercus_Raw_', species.name, '.csv')), row.names=F)


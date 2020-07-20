library(ggplot2)
site.id <- 'MortonArb'

path.npn <- "../data_raw/NPN/"

oak.leaf <- read.csv(file.path(path.npn, paste0('Quercus_', site.id, '.csv')))
oak.leaf$species <- as.factor(oak.leaf$species)
oak.leaf$species_id <- as.factor(oak.leaf$species_id)
oak.leaf$individual_id <- as.factor(oak.leaf$individual_id)
oak.leaf$phenophase_id <- as.factor(oak.leaf$phenophase_id)
oak.leaf$phenophase_description <- as.factor(oak.leaf$phenophase_description)
summary(oak.leaf)

summary(oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$last_yes_year==2019,])

oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$last_yes_year==2019 & oak.leaf$individual_id==132857,]

oak.leaf[oak.leaf$phenophase_id==371 & oak.leaf$last_yes_year==2017 & oak.leaf$individual_id==132848,]


ggplot(data=oak.leaf[oak.leaf$phenophase_id==371,]) +
  facet_wrap(~species, scales="free_y") +
  geom_linerange(aes(xmin=first_yes_doy, xmax=last_yes_doy, y=individual_id, color=as.factor(first_yes_year)), position=position_dodge(width=0.5), size=2) +
  labs(color="year", title="budburst") +
  theme(legend.position = "bottom")

png("../figures/TEST_NPN_QAQC_Leaves.png", height=6, width=7, units="in", res=180)
ggplot(data=oak.leaf[oak.leaf$phenophase_id==483,]) +
  facet_wrap(~species, scales="free_y") +
  geom_linerange(aes(xmin=first_yes_doy, xmax=last_yes_doy, y=individual_id, color=as.factor(first_yes_year)), position=position_dodge(width=0.5), size=2) +
  labs(color="year", title="leaves individual phenometrics") +
  theme(legend.position = "bottom")
dev.off()

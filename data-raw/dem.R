# Digital elevation model
# remotes::install_github("bcgov/bcmaps")
library(bcmaps)



#Skeena Region AOI
nr <- bcmaps::nr_regions()
skeena_aoi <- nr$geometry[which(nr$ORG_UNIT == "RSK")]


# This will cache the whole cded locally. It's a large download.
# For development purpose only, we will compress it a later stage
# for better efficiency.
dem <- cded(aoi = sf::st_bbox(skeena_aoi))

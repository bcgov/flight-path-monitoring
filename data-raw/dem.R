# Digital elevation model
# remotes::install_github("bcgov/bcmaps")
library(bcmaps)
# This will cache the whole cded locally. It's a large download.
# For development purpose only, we will compress it a later stage
# for better efficiency.
dem <- cded(aoi = bc_bbox())

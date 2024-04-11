# compare_annual_3yr_images.R
rm(list=ls())
library(dplyr)

#Cameroon in 2007
cmr_annual_terra_r <- terra::rast("./data/dhs_tifs_c1_5k_annual/cameroon_2004/00119.tif",
                                  lyrs=c("00119_43","00119_44","00119_45"))
terra::cellSize(cmr_annual_terra_r)

#year 2007
cmr_annual_rgb <-  cmr_annual_terra_r[[c(1,2,3)]]
cmr_annual_scaled <- cmr_annual_rgb/.0001
dev.off()
raster::plotRGB(cmr_annual_scaled,r=3,g=2,b=1)

cmr_3yr_terra_r <- terra::rast("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00119.tif",
                               lyrs=c("00119_13","00119_14","00119_15"))
terra::cellSize(cmr_3yr_terra_r)
sqrt(891.2482)
#years 2005-2007
cmr_3yr_rgb <-  cmr_3yr_terra_r[[c(1,2,3)]]
cmr_3yr_scaled <- cmr_3yr_rgb/.0001
dev.off()
raster::plotRGB(cmr_3yr_scaled,r=3,g=2,b=1)


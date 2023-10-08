# check_annual_images.R
rm(list=ls())
library(dplyr)
library(stringr)

# Validate the download of annual images, comparing them to 3-year images
#raster library
#lat/lon  -11.491992 43.38610
comoros_r <- raster::brick("./data/dhs_tifs/comoros_2012/00149.tif")
# class      : RasterBrick 
# dimensions : 224, 224, 50176, 80  (nrow, ncol, ncell, nlayers)
# resolution : 0.0005473618, 0.0005398282  (x, y)
# extent     : 43.32489, 43.4475, -11.55244, -11.43152  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00149.tif 
# names      : X00149_1, X00149_2, X00149_3, X00149_4, X00149_5, X00149_6, X00149_7, X00149_8, X00149_9, X00149_10, X00149_11, X00149_12, X00149_13, X00149_14, X00149_15, ... 
comoros_rgb <-  comoros_r[[c(1,2,3)]]
comoros_scaled <- comoros_rgb/.0001
raster::plotRGB(comoros_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(comoros_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(comoros_scaled,r=3,g=2,b=1,stretch="hist")

comoros_annual_r <- raster::brick("./data/dhs_tifs_annual/comoros_2012/00222.tif")
# class      : RasterBrick 
# dimensions : 167, 167, 27889, 84  (nrow, ncol, ncell, nlayers)
# resolution : 0.0002736804, 0.0002699141  (x, y)
# extent     : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00222.tif 
# names      : X00222_1, X00222_2, X00222_3, X00222_4, X00222_5, X00222_6, X00222_7, X00222_8, X00222_9, X00222_10, X00222_11, X00222_12, X00222_13, X00222_14, X00222_15, ... 

#year 2000
comoros_annual_rgb <-  comoros_annual_r[[c(1,2,3)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2001
comoros_annual_rgb <-  comoros_annual_r[[c(7,8,9)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000875
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2002
comoros_annual_rgb <-  comoros_annual_r[[c(13,14,15)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2003
comoros_annual_rgb <-  comoros_annual_r[[c(19,20,21)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2004
comoros_annual_rgb <-  comoros_annual_r[[c(25,26,27)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2005
comoros_annual_rgb <-  comoros_annual_r[[c(31,32,33)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2006
comoros_annual_rgb <-  comoros_annual_r[[c(37,38,39)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2007
comoros_annual_rgb <-  comoros_annual_r[[c(43,44,45)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2008
comoros_annual_rgb <-  comoros_annual_r[[c(49,50,51)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2009
comoros_annual_rgb <-  comoros_annual_r[[c(55,56,57)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2010
comoros_annual_rgb <-  comoros_annual_r[[c(61,62,63)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2011
comoros_annual_rgb <-  comoros_annual_r[[c(67,68,69)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2012
comoros_annual_rgb <-  comoros_annual_r[[c(73,74,75)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")

#year 2013
comoros_annual_rgb <-  comoros_annual_r[[c(79,80,81)]]
comoros_annual_scaled <- comoros_annual_rgb/.0000275
dev.off()
raster::plotRGB(comoros_annual_scaled,r=3,g=2,b=1,stretch="hist")


dhs_iwi <- read.csv("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/interim/dhs_est_iwi.csv")

dhs_iwi_annual <- dhs_iwi %>% 
  group_by(country, year) %>% 
  mutate(image_file_annual = paste0("./data/dhs_tifs_annual/",country,"_",year,"/",
                                    str_pad(row_number() - 1,width = 5, pad="0"),
                                    ".tif")) %>% 
  ungroup()

dhs_iwi_annual %>% 
  filter(image_file == "./data/dhs_tifs/comoros_2012/00149.tif") %>% 
  select(dhs_id, image_file_annual)
#8158 ./data/dhs_tifs_annual/comoros_2012/00222.tif

write.csv(dhs_iwi_annual,"/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/interim/dhs_est_iwi_annual.csv", row.names=FALSE)

#lat/lon -34.463232 19.54247
safrica_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00743.tif")
safrica_rgb <-  safrica_r[[c(1,2,3)]]
safrica_scaled <- safrica_rgb/.0001
raster::plotRGB(safrica_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="hist")

#lat/lon -34.418873 19.18893
safrica2_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00694.tif")
safrica2_rgb <-  safrica2_r[[c(1,2,3)]]
safrica2_scaled <- safrica2_rgb/.0001
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="hist")



# check_annual_images.R
rm(list=ls())
library(dplyr)
library(stringr)

# Validate the download of annual images, comparing them to 3-year images
#raster library
#lat/lon  -11.491992 43.38610
comoros_old_r <- raster::brick("./data/dhs_tifs/comoros_2012/00149.tif")
# class      : RasterBrick 
# dimensions : 224, 224, 50176, 80  (nrow, ncol, ncell, nlayers)
# resolution : 0.0005473618, 0.0005398282  (x, y)
# extent     : 43.32489, 43.4475, -11.55244, -11.43152  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00149.tif 
# names      : X00149_1, X00149_2, X00149_3, X00149_4, X00149_5, X00149_6, X00149_7, X00149_8, X00149_9, X00149_10, X00149_11, X00149_12, X00149_13, X00149_14, X00149_15, ... 
comoros_old_rgb <-  comoros_old_r[[c(1,2,3)]]
comoros_old_scaled <- comoros_old_rgb/.0001
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1)
dev.off()
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1,stretch="lin")
dev.off()
raster::plotRGB(comoros_old_scaled,r=3,g=2,b=1,stretch="hist")

comoros_annual_r <- raster::brick("./data/dhs_tifs_annual/comoros_2012/00222.tif")
# class      : RasterBrick 
# dimensions : 167, 167, 27889, 84  (nrow, ncol, ncell, nlayers)
# resolution : 0.0002736804, 0.0002699141  (x, y)
# extent     : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00222.tif 
# names      : X00222_1, X00222_2, X00222_3, X00222_4, X00222_5, X00222_6, X00222_7, X00222_8, X00222_9, X00222_10, X00222_11, X00222_12, X00222_13, X00222_14, X00222_15, ... 


comoros_annual_terra_r <- terra::rast("./data/dhs_tifs_annual/comoros_2012/00222.tif",
                                      lyrs=c("00222_1","00222_2","00222_3"))
terra::cellSize(comoros_annual_terra_r)
# class       : SpatRaster 
# dimensions  : 167, 167, 1  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source(s)   : memory
# name        :     area 
# min value   : 891.4497 
# max value   : 891.5878 


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

#get list of all dhs points and their tif file locations
dhs_iwi <- read.csv("./data/interim/dhs_est_iwi.csv")

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

#Sierra Leone
sl_old_terra_r <- terra::rast("./data/dhs_tifs/sierra_leone_2008/00013.tif",
                                 lyrs=c("00013_1","00013_2","00013_3"))
sl_old_rgb <-  sl_old_terra_r[[c(1,2,3)]]
sl_old_scaled <- sl_old_rgb/.0001
dev.off()
raster::plotRGB(sl_old_scaled,r=3,g=2,b=1,stretch="hist")

sl_annual_terra_r <- terra::rast("./data/dhs_tifs_annual/sierra_leone_2008/00161.tif",
                                      lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_annual_terra_r)
sqrt(891.2482)
#year 2000
sl_annual_rgb <-  sl_annual_terra_r[[c(1,2,3)]]
sl_annual_scaled <- sl_annual_rgb/.0000275
dev.off()
raster::plotRGB(sl_annual_scaled,r=3,g=2,b=1,stretch="hist")


sl_3yr_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr/sierra_leone_2008/00161.tif",
                                 lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_3yr_terra_r)
sqrt(891.2482)
#years 1999-2001
sl_3yr_rgb <-  sl_3yr_terra_r[[c(1,2,3)]]
sl_3yr_scaled <- sl_3yr_rgb/.0000275
dev.off()
raster::plotRGB(sl_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#46422	Sierra_leone	SLE	8.40611	-10.67349	0	0	2013	./data/dhs_tifs_annual/sierra_leone_2008/00161.tif
#./data/dhs_tifs/sierra_leone_2008/00013.tif

sl_3yr_v2_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr_v2/sierra_leone_2008/00161.tif",
                              lyrs=c("00161_1","00161_2","00161_3"))
terra::cellSize(sl_3yr_v2_terra_r)
sqrt(891.2482)
#years 1999-2001
sl_3yr_v2_rgb <-  sl_3yr_v2_terra_r[[c(1,2,3)]]
sl_3yr_v2_scaled <- sl_3yr_v2_rgb/.0000275
dev.off()
raster::plotRGB(sl_3yr_v2_scaled,r=3,g=2,b=1,stretch="hist")


comoros_5k_3yr_r <- raster::brick("./data/dhs_tifs_5k_3yr_v2/comoros_2012/00222.tif")
# class      : RasterBrick 
# dimensions : 167, 167, 27889, 36  (nrow, ncol, ncell, nlayers)
# resolution : 0.0002736804, 0.0002699141  (x, y)
# extent     : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : 00222.tif 
# names      : X00222_1, X00222_2, X00222_3, X00222_4, X00222_5, X00222_6, X00222_7, X00222_8, X00222_9, X00222_10, X00222_11, X00222_12, X00222_13, X00222_14, X00222_15, ... 


comoros_5k_3yr_terra_r <- terra::rast("./data/dhs_tifs_5k_3yr_v2/comoros_2012/00222.tif")
# class       : SpatRaster 
# dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source      : 00222.tif 
# names       : 00222_1, 00222_2, 00222_3, 00222_4, 00222_5, 00222_6, ... 

terra::cellSize(comoros_5k_3yr_terra_r)
# class       : SpatRaster 
# dimensions  : 167, 167, 1  (nrow, ncol, nlyr)
# resolution  : 0.0002736804, 0.0002699141  (x, y)
# extent      : 43.36329, 43.40899, -11.51453, -11.46945  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source(s)   : memory
# name        :     area 
# min value   : 891.4497 
# max value   : 891.5878 


#year 1999-2001
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(1,2,3)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275)+ 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#year 2002-2004
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(7,8,9)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275)+ 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")

#year 2005-2007
comoros_5k_3yr_rgb <-  comoros_5k_3yr_terra_r[[c(13,14,15)]]
comoros_5k_3yr_scaled <- ((comoros_5k_3yr_rgb /.0000275) + 0.2)
dev.off()
raster::plotRGB(comoros_5k_3yr_scaled,r=3,g=2,b=1,stretch="hist")



# check_3yr_images.R
rm(list=ls())
library(dplyr)
library(stringr)


dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv") %>% 
  select(dhs_id, lat, lon, country, image_file, image_file_annual, image_file_5k_3yr)

dhs_df %>% 
  filter(image_file_5k_3yr %in% c(
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00137.tif",
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00133.tif",
    "./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00151.tif"
  ))

dhs_df %>% 
  filter(country=="cameroon")

c1 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00409.tif")
c2 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00070.tif")
c3 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00067.tif")
c4 <- raster::brick("./data/dhs_tifs_5k_3yr/cameroon_2004/00062.tif")

c5 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00137.tif")  #2005:2007 bands 7-9
c6 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00133.tif") #2005:2007  bands 7-9
c7 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00151.tif") #2005:2007  bands 7-9

c5_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00431.tif")  #2005:2007   bands 41-43
c6_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00404.tif") #2005:2007    bands 41-43
c7_initial <- raster::brick("./data/dhs_tifs/cameroon_2004/00414.tif") #2005:2007    bands 41-43


c5_rgb <-  c5[[c(7,8,9)]]
c5_scaled <- ((c5_rgb + .2)/.0000275)
raster::plotRGB(c5_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	


c5_initial_rgb <-  c5_initial[[c(41,42,43)]]
c5_initial_scaled <- ((c5_initial_rgb)/.0001)
raster::plotRGB(c5_initial_scaled,r=3,g=2,b=1,stretch="hist")		
#put vertical/horizontal lines to create approximatly 5k box for comparisons
w = ncol(c5_initial_scaled)
abline(v=w/3,col="White")
abline(v=1 * w/4,col="White")
abline(h=w/3,col="White")
abline(h=2 * w/3,col="White")
dev.off()	



n1_c1_rgb <-  n1_c1[[c(1,2,3)]]
n1_c1_scaled <- (n1_c1_rgb/.0001)
raster::plotRGB(n1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	



n1 <- raster::brick("./data/dhs_tifs_5k_3yr/niger_1998/00074.tif") #2008:2010
n1_c2_v3 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/niger_1998/00074.tif")

c1_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00409.tif")
c2_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00070.tif")
c3_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00067.tif")
c4_c1 <- raster::brick("./data/dhs_tifs_c1_5k_3yr/cameroon_2004/00062.tif")
n1_v3 <- raster::brick("./data/dhs_tifs_5k_3yr_v3/niger_1998/00074.tif")

n1_rgb <-  n1[[c(1,2,3)]]
n1_scaled <- ((n1_rgb + .2)/.0000275)
raster::plotRGB(n1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 


n1_v3_rgb <-  n1_v3[[c(1,2,3)]]
n1_v3_scaled <- ((n1_v3_rgb + .2)/.0000275)
raster::plotRGB(n1_v3_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	

n1_c1_rgb <-  n1_c1[[c(1,2,3)]]
n1_c1_scaled <- (n1_c1_rgb/.0001)
raster::plotRGB(n1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()		


n1_200810_rgb <-  n1[[c(13,14,15)]]
n1_200810_scaled <- ((n1_200810_rgb + .2)/.0000275)
raster::plotRGB(n1_200810_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()	

n1_c1_200810_rgb <-  n1_c1[[c(13,14,15)]]
n1_c1_200810_scaled <- (n1_c1_200810_rgb/.0001)
raster::plotRGB(n1_c1_200810_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()		

######
c1_rgb <-  c1[[c(1,2,3)]]
c1_scaled <- ((c1_rgb + .2)/.0000275)
raster::plotRGB(c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c1_c1_rgb <-  c1_c1[[c(1,2,3)]]
c1_c1_scaled <- (c1_c1_rgb/.0001)
raster::plotRGB(c1_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					

######
c2_rgb <-  c2[[c(1,2,3)]]
c2_scaled <- ((c2_rgb + .2)/.0000275)
raster::plotRGB(c2_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c2_c1_rgb <-  c2_c1[[c(1,2,3)]]
c2_c1_scaled <- (c2_c1_rgb/.0001)
raster::plotRGB(c2_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  c3_rgb <-  c3[[c(1,2,3)]]
c3_scaled <- ((c3_rgb + .2)/.0000275)
raster::plotRGB(c3_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c3_c1_rgb <-  c3_c1[[c(1,2,3)]]
c3_c1_scaled <- c3_c1_rgb /.0001
raster::plotRGB(c3_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  c4_rgb <-  c4[[c(1,2,3)]]
c4_scaled <- ((c4_rgb + .2)/.0000275)
raster::plotRGB(c4_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()					 

c4_c1_rgb <-  c4_c1[[c(1,2,3)]]
c4_c1_scaled <- c4_c1_rgb/.0001
raster::plotRGB(c4_c1_scaled,r=3,g=2,b=1,stretch="hist")						 
dev.off()
######==
  
most_least_likely_v <- c('./data/dhs_tifs_5k_3yr/niger_1998/00074.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00072.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00074.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00072.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00183.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00182.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00056.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00053.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00059.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00040.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00204.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00194.tif',
                         './data/dhs_tifs_5k_3yr/comoros_2012/00201.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00027.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00001.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00214.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif',
                         './data/dhs_tifs_5k_3yr/lesotho_2004/00192.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00225.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00218.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00224.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00160.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00161.tif',
                         './data/dhs_tifs_5k_3yr/zimbabwe_1999/00132.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00061.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00062.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00061.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00225.tif',
                         './data/dhs_tifs_5k_3yr/senegal_1997/00218.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00409.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00070.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00067.tif',
                         './data/dhs_tifs_5k_3yr/cameroon_2004/00062.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00190.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00185.tif',
                         './data/dhs_tifs_5k_3yr/namibia_2000/00195.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00326.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00320.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00087.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00106.tif',
                          './data/dhs_tifs_5k_3yr/chad_2014/00090.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00190.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00195.tif',
                          './data/dhs_tifs_5k_3yr/namibia_2000/00185.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00071.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00069.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00074.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00326.tif',
                          './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00236.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00240.tif',
                          './data/dhs_tifs_5k_3yr/rwanda_2005/00229.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00176.tif',
                          './data/dhs_tifs_5k_3yr/niger_1998/00040.tif',
                          './data/dhs_tifs_5k_3yr/guinea_1999/00078.tif',
                         './data/dhs_tifs_5k_3yr/guinea_1999/00090.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00050.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00055.tif',
                         './data/dhs_tifs_5k_3yr/uganda_2000/00054.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00167.tif',
                         './data/dhs_tifs_5k_3yr/tanzania_1999/00012.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00220.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00055.tif',
                         './data/dhs_tifs_5k_3yr/angola_2006/00056.tif',
                         './data/dhs_tifs_5k_3yr/guinea_1999/00084.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00168.tif',
                         './data/dhs_tifs_5k_3yr/nigeria_2003/00328.tif',
                         './data/dhs_tifs_5k_3yr/niger_1998/00106.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00473.tif',
                         './data/dhs_tifs_5k_3yr/ethiopia_2000/00472.tif',
                         './data/dhs_tifs_5k_3yr/madagascar_1997/00115.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                         './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00168.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00030.tif',
                        './data/dhs_tifs_5k_3yr/cameroon_2004/00097.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00038.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00097.tif',
                        './data/dhs_tifs_5k_3yr/ghana_1999/00055.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00221.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00219.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00221.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00012.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00001.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00003.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00256.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00266.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00276.tif',
                        './data/dhs_tifs_5k_3yr/mali_1996/00158.tif',
                        './data/dhs_tifs_5k_3yr/mali_1996/00157.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00001.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00003.tif',
                        './data/dhs_tifs_5k_3yr/benin_1996/00008.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00168.tif',
                        './data/dhs_tifs_5k_3yr/zambia_2007/00171.tif',
                        './data/dhs_tifs_5k_3yr/angola_2006/00027.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00005.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00003.tif',
                        './data/dhs_tifs_5k_3yr/eswatini_2006/00151.tif',
                        './data/dhs_tifs_5k_3yr/lesotho_2004/00118.tif',
                        './data/dhs_tifs_5k_3yr/nigeria_2003/00351.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/chad_2014/00220.tif',
                        './data/dhs_tifs_5k_3yr/angola_2006/00001.tif') 
#exclude duplicates
most_least_likely_v <-  unique(most_least_likely_v)

#get other DHS attributes, limit to those in vector above
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv") %>% 
  filter(image_file_5k_3yr %in% most_least_likely_v) %>% 
  select(dhs_id, lat, lon, country, image_file, image_file_annual, image_file_5k_3yr)

year_group_v <- c('2002:2004', '2005:2007', '2008:2010', '2011:2013', '2014:2016')

for (i in 1:nrow(dhs_df)) {
  i=1
  for (oda_year_group in year_group_v) {
    oda_year_group = '2002:2004'
    bands_to_select5k <- case_when(							  
      oda_year_group == '2002:2004' ~ 1:3,
      oda_year_group == '2005:2007' ~ 7:9,
      oda_year_group == '2008:2010' ~ 13:15,
      oda_year_group == '2011:2013' ~ 19:21,
      oda_year_group == '2014:2016' ~ 25:27)
    
    # iterate over all image bands to construct their names
    image_5k_name <-dhs_df[i,"image_file_5k_3yr"]
    #image_name = './data/dhs_tifs_5k_3yr/chad_2014/00220.tif'
    image_short_name <- gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_5k_name)
    
    layers5k=paste(image_short_name, bands_to_select5k,"_")
    for(i in 1:3) {
      band_ <- bands_to_select5k[i]
      layers5k=paste0(layers5k,", \'",gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_5k_name)
                  ,"_",band_)
    }
    #remove leading comma space
    layers5k=paste0(sub("^, ", "", layers5k),"'")
    
    #read the image
    im <- terra::rast(image_5k_name, lyrs=c(!!sym(layers5k)))  
    
    
    im <- terra::rast(image_5k_name, lyrs=c("00118_1", "00118_2", "00118_3"))  
    # class       : SpatRaster 
    # dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
    # resolution  : 0.0003075762, 0.0002699141  (x, y)
    # extent      : 27.44171, 27.49308, -29.33524, -29.29016  (xmin, xmax, ymin, ymax)
    # coord. ref. : lon/lat WGS 84 (EPSG:4326) 
    # source      : 00211.tif 
    # names       : 00211_1, 00211_2, 00211_3, 00211_4, 00211_5, 00211_6, ... 
    
    
    im <- terra::rast(image_5k_name,
                      lyrs=c("00211_1", "00211_2", "00211_3"))  
    # Error: [rast] cannot open this file as a SpatRaster: /mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif
    # In addition: Warning message:
    #   In new_CppObject_xp(fields$.module, fields$.pointer, ...) :
    #   GDAL Error 4: `/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr/lesotho_2004/00211.tif' not recognized as a supported file format.
    
    im <- terra::rast("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/data/dhs_tifs_5k_3yr_v3/lesotho_2004/00211.tif")
    
    # class       : SpatRaster 
    # dimensions  : 167, 167, 36  (nrow, ncol, nlyr)
    # resolution  : 0.0003075762, 0.0002699141  (x, y)
    # extent      : 27.44171, 27.49308, -29.33524, -29.29016  (xmin, xmax, ymin, ymax)
    # coord. ref. : lon/lat WGS 84 (EPSG:4326) 
    # source      : 00211.tif 
    # names       : 00211_1, 00211_2, 00211_3, 00211_4, 00211_5, 00211_6, ... 
    

    #rescale to original setting for RGB printing
    im <- ((im + .2)/.0000275)
    
    #plot
    # Output or intermediate results for review
    print(paste("5k:",image_5k_name,oda_year_group))
    raster::plotRGB(im,r=3,g=2,b=1,stretch="hist")
    
    #pause until I review
    cat("Press Enter to continue...")
    readline()
    
    #now show larger footprint file
    
    
  }
}





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



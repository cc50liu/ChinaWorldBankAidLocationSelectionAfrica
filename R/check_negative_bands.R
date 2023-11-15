# check_neg_values.R
rm(list=ls())
library(dplyr)
library(stringr)
library(raster)

dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")  %>% 
  dplyr::select(dhs_id, lat, lon, country, image_file, image_file_annual, image_file_5k_3yr)  

neg_values_df <- read.csv("./data/interim/images_w_neg_values.csv")

dhs_neg_df <- neg_values_df %>% 
  left_join(dhs_df,by=join_by(dhs_id)) %>% 
  mutate(year_group=case_when(image_type="annual" & layer %in% 1:6   ~ "2000",
                        image_type="annual" & layer %in% 7:12  ~ "2001",
                        image_type="annual" & layer %in% 13:18 ~ "2002",
                        image_type="annual" & layer %in% 19:24 ~ "2003",
                        image_type="annual" & layer %in% 25:30 ~ "2004",
                        image_type="annual" & layer %in% 31:36 ~ "2005",
                        image_type="annual" & layer %in% 37:42 ~ "2006",
                        image_type="annual" & layer %in% 43:48 ~ "2007",
                        image_type="annual" & layer %in% 49:54 ~ "2008",
                        image_type="annual" & layer %in% 55:60 ~ "2009",
                        image_type="annual" & layer %in% 61:66 ~ "2010",
                        image_type="annual" & layer %in% 67:72 ~ "2011",
                        image_type="annual" & layer %in% 73:78 ~ "2012",
                        image_type="annual" & layer %in% 79:84 ~ "2013",
                        image_type="3yr" & layer %in% 1:6 ~ "1999-2001",
                        image_type="3yr" & layer %in% 7:12 ~ "2002-2004",
                        image_type="3yr" & layer %in% 13:18 ~ "2005-2007",
                        image_type="3yr" & layer %in% 19:24 ~ "2008-2010",
                        image_type="3yr" & layer %in% 25:30 ~ "2011-2013",
                        image_type="3yr" & layer %in% 31:36 ~ "2014-2016",
                        image_type="original" & layer %in% 1:8 ~ "1990-1992",
                        image_type="original" & layer %in% 9:16 ~ "1993-1995",
                        image_type="original" & layer %in% 17:24 ~ "1996-1998",
                        image_type="original" & layer %in% 25:32 ~ "1999-2001",
                        image_type="original" & layer %in% 33:40 ~ "2002-2004",
                        image_type="original" & layer %in% 41:48 ~ "2005-2007",
                        image_type="original" & layer %in% 49:56 ~ "2008-2010",
                        image_type="original" & layer %in% 57:64 ~ "2011-2013",
                        image_type="original" & layer %in% 65:72 ~ "2014-2016"),
         band=case_when(image_type="annual" & layer %in% c(1,7,13,19,25,31,37,43,49,55,61,67,73,79) ~ "BLUE",
                        image_type="annual" & layer %in% c(2,8,14,20,26,32,38,44,50,56,62,68,74,80) ~ "GREEN",
                        image_type="annual" & layer %in% c(3,9,15,21,27,33,39,45,51,57,63,69,75,81) ~ "RED",
                        image_type="annual" & layer %in% c(4,10,16,22,28,34,40,46,52,58,64,70,76,82) ~ "NIR",
                        image_type="annual" & layer %in% c(5,11,17,23,29,35,41,47,53,59,65,71,77,83) ~ "SWIR1",
                        image_type="annual" & layer %in% c(6,12,18,24,30,36,42,48,54,60,66,72,78,84) ~ "SWIR2",
                        image_type="3yr" & layer %in% c(1,7,13,19,25,31) ~ "BLUE",
                        image_type="3yr" & layer %in% c((2,8,14,20,26,32) ~ "GREEN",
                        image_type="3yr" & layer %in% c(3,9,15,21,27,33) ~ "RED",
                        image_type="3yr" & layer %in% c(4,10,16,22,28,34) ~ "NIR",
                        image_type="3yr" & layer %in% c(5,11,17,23,29,35) ~ "SWIR1",
                        image_type="3yr" & layer %in% c(6,12,18,24,30,36) ~ "SWIR2",           
                        image_type="original" & layer %in% c(1,9,17,25,33,41,49,57,65,73) ~ "BLUE",
                        image_type="original" & layer %in% c(2,10,18,26,34,42,50,58,66,74) ~ "GREEN",
                        image_type="original" & layer %in% c(3,11,19,27,35,43,51,59,67,75) ~ "RED",
                        image_type="original" & layer %in% c(4,12,20,28,36,44,52,60,68,76) ~ "NIR",
                        image_type="original" & layer %in% c(5,13,21,29,37,45,53,61,69,77) ~ "SWIR1",
                        image_type="original" & layer %in% c(6,14,22,30,38,46,54,62,70,78) ~ "SWIR2",
                        image_type="original" & layer %in% c(67,15,23,31,39,47,55,63,71,79) ~ "TEMP",
                        image_type="original" & layer %in% c(8,16,24,32,40,48,56,64,72,80) ~ "NL")
                        )
  )
                        
                        


year_groups <- c("2002-2004","2005-2007","2008-2010","2011-2013")
original_r <- c(35,43,51,59)
original_g <- c(34,42,50,58)
original_b <- c(33,41,49,57)
five_k_3yr_r  <- c(9,15,21,27)
five_k_3yr_g  <- c(8,14,20,26)
five_k_3yr_b  <- c(7,13,19,25)
#for annual rows, just look at the first year of the group
annual_r <- c(15,33,51,69)
annual_g <- c(14,32,50,68)
annual_b <- c(13,31,49,67)


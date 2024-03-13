#prep_dhs_points.R
library(dplyr)
library(stringr)
library(sf)

rm(list=ls())

dhs_df <- read.csv("./data/AIGlobalLab/dhs_clusters.csv") 

#assign an id to each survey location
#start with 0 to match numbers assigned in Markus' code 
dhs_df$dhs_id <- 0:(nrow(dhs_df)-1) 

#add an ISO3 column
dhs_df <- dhs_df %>%
  mutate(iso3 = substr(GID_1, 1, 3))

write.csv(dhs_df,"./data/interim/dhs_clusters_id.csv",row.names=FALSE)
#dhs_df <- read.csv("./data/interim/dhs_clusters_id.csv")

#define a subset that will be used for treatments and controls
#first, create a dataframe with year, iso3, and country name of control surveys
#for countries with surveys prior to 2000, use the max year closest to 2000
iso3_year <- dhs_df %>% 
    filter(year < 2000) %>%
    group_by(iso3) %>%
    arrange(desc(year)) %>%
    filter(row_number()==1) %>%
    select(year,iso3,country) %>% 
    ungroup()
  
#for all other countries, use the first survey year >=2000
post_iso3_year <- dhs_df %>% 
    filter(!iso3 %in% iso3_year$iso3) %>% 
    group_by(iso3) %>%
    arrange(year) %>%
    filter(row_number()==1) %>%
    select(year,iso3,country) %>% 
    ungroup()

dhs_t_c_year <- rbind(iso3_year,post_iso3_year)

#change cameroon from 1991 to 2004, to avoid duplicate lat/lons
#CAF 2004 also has duplicates, but also no better option
dhs_t_c_year <- dhs_t_c_year %>% 
  mutate(year=case_match(iso3,
                         "CMR" ~ 2004,
                         .default=year))

#write the list of years to a file
write.csv(dhs_t_c_year,"./data/interim/dhs_treat_control_year.csv",row.names=FALSE)
#dhs_t_c_year <- read.csv("./data/interim/dhs_treat_control_year.csv")
rm(iso3_year,post_iso3_year)

#create a subset of DHS records that will be used for treatments and controls
dhs_t_c_df <- semi_join(dhs_df, dhs_t_c_year, by=c("year","iso3")) 

#test for duplicates
dhs_t_c_df %>%
  filter((duplicated(lat, lon))) %>%
  group_by(country, iso3, year) %>%
  distinct(country, iso3, year)
# country                  iso3   year
# <chr>                    <chr> <int>
# 1 cameroon                 CMR    1991  #changed to 2004
# 2 central_african_republic CAF    1995  #no better year

#include outcome estimates
#csv outcome estimates from Markus over DHS points
dhs_est_iwi_df <- read.csv("./data/AIGlobalLab/incountry/bidirectional_resnet_lstm.csv") %>% 
  rename(dhs_id=X)

#add outcome values to dataframe 
dhs_tc_est_df <- left_join(dhs_t_c_df,
                    dhs_est_iwi_df,
                    by="dhs_id") %>% 
  select(-fold,-GID_1,-GID_2,-y_i) %>% 
  rename(iwi_1990_1992_est=y_0,
         iwi_1993_1995_est=y_1,
         iwi_1996_1998_est=y_2,
         iwi_1999_2001_est=y_3,
         iwi_2002_2004_est=y_4,
         iwi_2005_2007_est=y_5,
         iwi_2008_2010_est=y_6,
         iwi_2011_2013_est=y_7,
         iwi_2014_2016_est=y_8,
         iwi_2017_2019_est=y_9
  )  %>% 
  #remove rows that don't have the post-project wealth estimates we need
  filter(!rowSums(across(starts_with("iwi"),is.na)) > 0) %>% 
  mutate(across(starts_with("iwi_"),~ . * 100))

#verify that wealth estimate consistent across duplicate lat/lon points
dhs_tc_est_df %>%
  filter(iso3=="CAF") %>%
  group_by(year,lat,lon,iwi_2017_2019_est) %>%
  count() %>%
  filter(n > 1)

#Pre n: 9977, post n: 9910
#retain only one DHS point for each duplicated lat/lon
dhs_tc_est_df <- dhs_tc_est_df %>%
  group_by(lat,lon) %>%
  slice_head()

#write.csv(dhs_tc_est_df,"./data/interim/dhs_est_iwi.csv",row.names=FALSE)
#dhs_tc_est_df <- read.csv("./data/interim/dhs_est_iwi.csv")

dhs_iwi_5k <- dhs_tc_est_df %>% 
  group_by(country, year) %>% 
  mutate(image_file_annual = paste0("./data/dhs_tifs_c1_5k_annual/",country,"_",year,"/",
                                    str_pad(row_number() - 1,width = 5, pad="0"),
                                    ".tif"),
    image_file_5k_3yr = paste0("./data/dhs_tifs_c1_5k_3yr/",country,"_",year,"/",
                                    str_pad(row_number() - 1,width = 5, pad="0"),
                                    ".tif")
  ) %>% 
  ungroup()


#############################################################
#### Lookup adm2 for each dhs point for use in fixed effects
#############################################################
projection <- "ESRI:102023"   #WGS 1984 Equidistant Conic for Africa.
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

#load administrative borders
adm2_borders <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- sf::st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
unique(sf::st_is_valid(adm2_borders))

#create an sf version of the dhs points
dhs_sf <- dhs_iwi_5k  %>%
  st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,year,iso3,rural,geometry) 

dhs_sf <- sf::st_transform(dhs_sf,crs=st_crs(projection))
dhs_sf <- sf::st_make_valid(dhs_sf)
unique(sf::st_is_valid(dhs_sf))

#do a spatial join between dhs points and adm2 borders
dhs_adm2_sf <- st_join(dhs_sf,
        adm2_borders,
        left=FALSE)

#If some locations are outside official ADM2 borders (like in a bay), 
#find nearest using the nngeo package
if (nrow(dhs_adm2_sf) != nrow(dhs_sf)) {
  missing_dhs_ids <- !(dhs_sf$dhs_id %in% dhs_adm2_sf$dhs_id)
  missing_sf <- dhs_sf[missing_dhs_ids, ]
  #make a version of the adm2 object with no geometry for field updates
  adm2_borders_no_geo <- adm2_borders %>% st_drop_geometry()
  
  missing_nn_sf <- missing_sf %>% 
    mutate(missing_index = unlist(nngeo::st_nn(.,adm2_borders)),
           ID_adm2 = adm2_borders_no_geo[missing_index,"ID_adm2"],
           ISO = adm2_borders_no_geo[missing_index,"ISO"],
           part_area = adm2_borders_no_geo[missing_index,"part_area"],
           adm2_geometry = st_geometry(adm2_borders[missing_index, ]),
           geometry = adm2_geometry) 
  dhs_adm2_sf <- rbind(dhs_adm2_sf,
                       missing_nn_sf %>% 
                         select(dhs_id,year,iso3,rural, geometry,ID_adm2,ISO,
                                part_area))
  
}
#convert sf to df object with columns we need for join with other dhs_attributes
dhs_adm2_df <- dhs_adm2_sf %>% 
  st_drop_geometry() %>% 
  select(dhs_id, ID_adm2, part_area)

#join with other dhs_attributes
dhs_iwi_adm2_5k <- dhs_iwi_5k %>% 
  inner_join(dhs_adm2_df,by="dhs_id")

write.csv(dhs_iwi_adm2_5k,"./data/interim/dhs_est_iwi.csv", row.names=FALSE)
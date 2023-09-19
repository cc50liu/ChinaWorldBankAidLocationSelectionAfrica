#prep_confounders_dhs_natl_res.R
#uses sf and nngeo libraries
library(dplyr)
rm(list=ls())

################################################
### Load general data
#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
projection <- "ESRI:102023"

#country boundaries, for map and FIPS id
gadm0_sf <- sf::st_read("./data/country_regions/gadm28_adm0.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(gadm0_sf) = "EPSG:4326"
gadm0_sf <- sf::st_transform(gadm0_sf,crs=sf::st_crs(projection))
gadm0_sf <- sf::st_make_valid(gadm0_sf)
unique(sf::st_is_valid(gadm0_sf))

################################################
### get DHS points 
dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, lat, lon)
  
#convert to sf object
dhs_sf <- dhs_df %>% 
  sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

################################################
### Gold
gold_sf <- readxl::read_excel("./data/GOLDDATA/Goldata_1.2.xlsx") %>% 
  #limit to Africa (include Egypt which is listed as middle-east instead of Africa)
  filter(CONT=="AF" | COUNTRY=="Egypt") %>%  
  select(PRIMKEY, COUNTRY, NAME, DISCyear, LONG, LAT) %>% 
  sf::st_as_sf(coords = c("LONG", "LAT"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

#create two sets of distances to account for the fact that one mine was discovered
#during the treatment period, in 2001
#PRIMKEY COUNTRY NAME      LAT  LONG CONT  COWcode AU_INFO OPERAT_I…¹ RES_I…² MINE_…³ PRODy…⁴
#  1 CD001AU Chad    Ganbokè  12.1  15.0 AF        483 P       Surface    NA      Produc…    2001

gold_pre2001_sf <- gold_sf[gold_sf$PRIMKEY!="CD001AU",]

dhs_gold_df <- dhs_sf %>%
  mutate(dist_km_to_gold_2001 = unlist(nngeo::st_nn(.,gold_sf,k=1,returnDist = TRUE)$dist) / 1000) %>%
  mutate(log_dist_km_to_gold_2001 = log(dist_km_to_gold_2001 + 1)) %>%
  mutate(dist_km_to_gold_pre2001 = unlist(nngeo::st_nn(.,gold_pre2001_sf,k=1,returnDist = TRUE)$dist) / 1000) %>%
  mutate(log_dist_km_to_gold_pre2001 = log(dist_km_to_gold_pre2001 + 1)) %>%
  sf::st_drop_geometry()

################################################
### Gems
gems_sf <- sf::read_sf("./data/GEMDATA/GEMDATA.shp") %>%  
  select(PRIMKEY, COUNTRY, FIPS, NAME, DISC_Y, LONGITUDE, LATITUDE, RUBY, SAPPHIRE,
        EMERALD, AQUAMARINE, HELIODOR, MOGANITE, GOSHENITE, NEPHRITE, JADEITE, 
        LAPIS_LAZU, OPAL, TOURMALINE, PERIODIT, TOPAZ, PEARL, GARNET, ZIRCON,
        SPINEL, AMBER, QUARZ) %>% 
  #no ISO country field, limit to countries with FIPS in the Africa shapefile 
  filter(FIPS %in% gadm0_sf$FIPS) %>% 
  sf::st_as_sf(coords = c("LONG", "LAT"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

#n=255, 0 discovered after 1999
dhs_gems_df <- dhs_sf %>%
  mutate(dist_km_to_gems = unlist(nngeo::st_nn(.,gems_sf,k=1,returnDist = TRUE)$dist) / 1000) %>%
  mutate(log_dist_km_to_gems = log(dist_km_to_gems + 1)) %>%
  sf::st_drop_geometry()

################################################
### Diamonds
dia_sf <- readxl::read_excel("./data/DIADATA/DIADATA_Excel_file.xls") %>% 
  #limit to Africa (include Egypt which is listed as middle-east instead of Africa)
  filter(CONTCODE==4 | COUNTRY=="Egypt") %>%  
  select(PRIMKEY, COUNTRY, NAME, DISC, LONG, LAT) %>% 
  sf::st_as_sf(coords = c("LONG", "LAT"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

#n=477, 0 discovered after 1999
dhs_dia_df <- dhs_sf %>%
  mutate(dist_km_to_dia = unlist(nngeo::st_nn(.,dia_sf,k=1,returnDist = TRUE)$dist) / 1000) %>%
  mutate(log_dist_km_to_dia = log(dist_km_to_dia + 1)) %>%
  sf::st_drop_geometry()

################################################
### Petroleum 
petro_off_sf <- sf::read_sf("./data/PETRODATA/Petrodata_offshore_V1.2.shp") %>% 
  #limit to Africa (include Egypt which is listed as middle-east instead of Africa)
  filter(CONTCODE==4 | COUNTRY=="Egypt") %>%  
  select(PRIMKEY, COUNTRY, NAME, DISC, LONG, LAT) %>% 
  sf::st_as_sf(coords = c("LONG", "LAT"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

#two sites discovered in 2000 at same time as project start
# PRIMKEY  COUNTRY      NAME                  DISC  LONG   LAT                        geometry
# * <chr>    <chr>        <chr>                <int> <dbl> <dbl>              <MULTIPOLYGON [m]>
# 1 OF311PET South Africa Orange River Coastal  2000  16.6 -30.8 (((-861061 -3391657, -859362.5…
# 2 OF310PET South Africa Orange River Coastal  2000  17.2 -30.6 (((-784407.5 -3367604, -784096…
                                                                                                                                      
petro_on_sf <- sf::read_sf("./data/PETRODATA/Petrodata_Onshore_V1.2.shp") %>%   
#limit to Africa (include Egypt which is listed as middle-east instead of Africa)
  filter(CONTCODE==4 | COUNTRY=="Egypt") %>%  
    select(PRIMKEY, COUNTRY, NAME, DISC, LONG, LAT) %>% 
    sf::st_as_sf(coords = c("LONG", "LAT"),crs="EPSG:4326") %>%
    sf::st_transform(crs=sf::st_crs(projection))

#one site discovered in Chad in 2003
# petro_on_sf %>% 
#   filter(DISC > 1999)
# PRIMKEY  COUNTRY NAME   DISC  LONG   LAT                                            geometry
# * <chr>    <chr>   <chr> <int> <dbl> <dbl>                                  <MULTIPOLYGON [m]>
#   1 CD002PET Chad    Sud    2003  16.4  10.9 (((-899826.7 1238306, -898029 1238253, -896237.9 1…
               
#n=142, 3 discovered after 1999, make different sets for each
petro_sf <- rbind(petro_off_sf, petro_on_sf)
petro_2000_2002_sf <- petro_sf[!petro_sf$PRIMKEY=="CD002PET",]
#petro_1999_sf <- petro_2000_2002_sf[!petro_2000_2002_sf$PRIMKEY %in% c("OF311PET","OF310PET"),]

dhs_petro_df <- dhs_sf %>% 
  mutate(dist_km_to_petro_2003 = unlist(nngeo::st_nn(.,petro_sf,k=1,returnDist = TRUE)$dist) / 1000) %>% 
  mutate(log_dist_km_to_petro_2003 = log(dist_km_to_petro_2003 + 1)) %>% 
  mutate(dist_km_to_petro_2000_2002 = unlist(nngeo::st_nn(.,petro_2000_2002_sf,k=1,returnDist = TRUE)$dist) / 1000) %>% 
  mutate(log_dist_km_to_petro_2000_2002 = log(dist_km_to_petro_2000_2002 + 1)) %>% 
  #mutate(dist_km_to_petro_1999 = unlist(nngeo::st_nn(.,petro_1999_sf,k=1,returnDist = TRUE)$dist) / 1000) %>% 
  #mutate(log_dist_km_to_petro_1999 = log(dist_km_to_petro_1999 + 1)) %>% 
  sf::st_drop_geometry()
# Warning message:
#   In withCallingHandlers(expr, warning = function(w) if (inherits(w,  :
#      NAs introduced by coercion

#couldn't find any NAs
dhs_petro_df %>% 
  filter(if_any(-dhs_id, ~is.na(.)))

#124 points have different distances in different timeframes
dhs_petro_df %>% 
  filter(log_dist_km_to_petro_2003 != log_dist_km_to_petro_2000_2002 |
         log_dist_km_to_petro_2000_2002 != log_dist_km_to_petro_2003) %>% 
  count()

dhs_petro_df %>% 
  filter(log_dist_km_to_petro_2003 != log_dist_km_to_petro_2000_2002 |
           log_dist_km_to_petro_2000_2002 != log_dist_km_to_petro_2003) %>% 
  select(dhs_id, starts_with("log"))

################################################
### Consolidate them all and write to a file
dhs_natl_res_df <- dhs_gold_df %>% 
  left_join(dhs_gems_df, by="dhs_id") %>% 
  left_join(dhs_dia_df, by="dhs_id") %>%
  left_join(dhs_petro_df, by="dhs_id")

write.csv(dhs_natl_res_df,"./data/interim/dhs_natl_res.csv",row.names=FALSE)  
#dhs_natl_res <-  read.csv("./data/interim/dhs_natl_res.csv") 

################################################
### Map them all and create density plots
library(tmap)
tmap_options(check.and.fix = TRUE)
africa_map_isos_df <- read.csv("./data/interim/africa_map_isos.csv")

#subset the data to exclude islands for mapping
gadm0_map_sf <- gadm0_sf[gadm0_sf$ISO %in% africa_map_isos_df$iso3, ]

natl_res_map <- tm_shape(gadm0_map_sf) +
  tm_borders() +
  tm_shape(petro_sf) + 
  tm_fill(col="gray60", labels="Oil") +
  tm_shape(gold_sf) + 
  tm_symbols(col="gold",size=.2,alpha=.8,shape=22, labels="Gold") +
  tm_shape(gems_sf) + 
  tm_symbols(col="purple",size=.2,alpha=.8,shape=21, labels="Gems") +
  tm_shape(dia_sf) + 
  tm_symbols(col="lightblue",size=.2,alpha=.8,shape=23, labels="Diamonds") +
  tm_add_legend(type = "fill"
                , col = c("gray60","gold","purple","lightblue")
                , labels = c("Oil","Gold","Gems","Diamonds"))  +
  tm_layout(main.title.size=1,
            main.title.position=c("center","top"),
            main.title = "Africa natural resource locations",
            legend.width=1,
            legend.text.size=.9) 

natl_res_map

tmap_save(natl_res_map, "./figures/africa_natl_res_map.png")

names(dhs_natl_res_df)

#create density plots
library(ggplot2)
nr_density <- dhs_natl_res_df %>% 
  tidyr::pivot_longer(starts_with("dist"),names_to="variable_names",values_to="Distance") %>% 
  ggplot(aes(Distance)) +
  geom_density() +
  facet_wrap(~ variable_names, labeller = labeller(variable_names = 
                          c(dist_km_to_gold_pre2001 = "Gold (pre-2001)",
                            dist_km_to_gold_2001 = "Gold (>= 2001)",
                            dist_km_to_gems = "Gems",
                            dist_km_to_dia = "Diamonds",
                            #dist_km_to_petro_1999 = "Oil (1999)",
                            dist_km_to_petro_2000_2002 = "Oil (2000-2002)",
                            dist_km_to_petro_2003 = "Oil (>= 2003)"))) +
  labs(title="Distance to Natural Resources from DHS",
       x="Distance (km)", y="Density") +
  theme_bw()
  
ggsave("./figures/nr_density.png",nr_density, width=4, height = 4, dpi=300,
       bg="white", units="in")

nr_log_density <- dhs_natl_res_df %>% 
  tidyr::pivot_longer(starts_with("log"),names_to="variable_names",values_to="Distance") %>% 
  ggplot(aes(Distance)) +
  geom_density() +
  facet_wrap(~ variable_names, labeller = labeller(variable_names = 
                                            c(log_dist_km_to_gold_pre2001 = "Gold (pre-2001)",
                                              log_dist_km_to_gold_2001 = "Gold (>= 2001)",
                                              log_dist_km_to_gems = "Gems",
                                              log_dist_km_to_dia = "Diamonds",
                                              #log_dist_km_to_petro_1999 = "Oil (1999)",
                                              log_dist_km_to_petro_2000_2002 = "Oil (2000-2002)",
                                              log_dist_km_to_petro_2003 = "Oil (>= 2003)"))) +                                              
  labs(title="Distance (log km) to Resources from DHS",
       x="Log Distance (km)", y="Density") +
  theme_bw()

ggsave("./figures/nr_log_density.png",nr_log_density, width=4, height = 4, dpi=300,
       bg="white", units="in")

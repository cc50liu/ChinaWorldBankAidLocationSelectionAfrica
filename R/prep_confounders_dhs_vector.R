#prep_confounders_dhs_vector.R
#uses sf libraries
library(dplyr)
library(ggplot2)
rm(list=ls())

####################################
# Conflict Deaths, limit to precise-enough precisions for subnational use
ucdp_p4_df <- read.csv("./data/UCDP/GEDEvent_v23_1.csv") %>% 
  filter(region=="Africa" &
         year %in% 1995:1999 & 
         where_prec <= 4)

#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
projection <- "ESRI:102023"

### Load administrative borders
#boundaries used in Gehring
gadm0_sf <- sf::st_read("./data/country_regions/gadm28_adm0.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(gadm0_sf) = "EPSG:4326"
gadm0_sf <- sf::st_transform(gadm0_sf,crs=sf::st_crs(projection))
gadm0_sf <- sf::st_make_valid(gadm0_sf)
unique(sf::st_is_valid(gadm0_sf))

gadm1_sf <- sf::st_read("./data/country_regions/gadm1_clean.shp") %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(gadm1_sf) = "EPSG:4326"
gadm1_sf <- sf::st_transform(gadm1_sf,crs=sf::st_crs(projection))
gadm1_sf <- sf::st_make_valid(gadm1_sf)  
unique(sf::st_is_valid(gadm1_sf))

#### Convert conflict deaths dataframe to sf
ucdp_p4_sf <- ucdp_p4_df  %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>%
  select(id, best, year, where_prec, adm_1, adm_2, geometry) %>%   
  sf::st_transform(crs=sf::st_crs(projection))

  #Do a spatial join to identify adm1 of all obs
  #this keeps the geometry of the ucdp point data
  ucdp_p4_adm1_sf <- ucdp_p4_sf %>% 
    sf::st_join(gadm1_sf, join = sf::st_intersects, left=FALSE) 
  #returns 5589 of 5640 obs
  
  #use a nearest_feature spatial join to look up the rest
  missing_sf <- ucdp_p4_sf[!ucdp_p4_sf$id %in% ucdp_p4_adm1_sf$id, ]
  missing_adm1_sf <- missing_sf %>% 
    sf::st_join(gadm1_sf, join = sf::st_nearest_feature, left=FALSE) 
  
  ucdp_p4_adm1_sf <- bind_rows(ucdp_p4_adm1_sf,
                               missing_adm1_sf)
  
  #store in a df
  ucdp_p4_adm1_df <- ucdp_p4_adm1_sf %>% 
    sf::st_drop_geometry() 
  
  #sum deaths by adm1
  ucdp_adm1_sum_df <- ucdp_p4_adm1_df %>% 
    group_by(ID_adm1, ISO) %>% 
    summarize(deaths1995_1999 = sum(best))  %>% 
    ungroup()
  #285 obs
  
  #create a density plot to look at the distribution of the values
  udcp_density <- ucdp_adm1_sum_df %>% 
    ggplot(aes(deaths1995_1999)) +
    geom_density() +
    labs(x="Total Deaths (prec<=4)", y="Density across ADM1s",
         title="African Conflict deaths 1995-1999 by ADM1")
  #highy right skewed - use the log
  udcp_density
  
  ggsave("./figures/udcp_density.png",udcp_density, width=6, height = 4, dpi=300,
      bg="white", units="in")
  
  ucdp_adm1_sum_df %>% 
    filter(deaths1995_1999 <= 0)
  #12 rows have 0 deaths, set those to 0 in the log
  
  ucdp_adm1_log_sum_df <- ucdp_adm1_sum_df %>% 
    mutate(log_deaths1995_1999 = ifelse(!is.finite(log(deaths1995_1999)), 0, 
                                        log(deaths1995_1999)))
    
  #density plot of the log
  udcp_log_density <-  ucdp_adm1_log_sum_df %>% 
    ggplot(aes(log_deaths1995_1999)) +
    geom_density() +
    labs(x="Log Total Deaths (prec<=4)", y="Density across ADM1s",
         title="Log of African Conflict deaths 1995-1999 by ADM1")

  udcp_log_density
  
  ggsave("./figures/udcp_log_density.png",udcp_log_density, width=6, height = 4, dpi=300,
         bg="white", units="in")
  
  # ucdp_adm1_sum_df %>% 
  #   select(ID_adm1, deaths1995_1999) %>% 
  #   filter(ISO=="COD")
  # 
  # ucdp_deaths_adm1 <- ucdp_p4_adm1_df %>% 
  #   group_by(ISO, .groups = 'drop') %>% 
  #   summarize(deaths1995_1999 = sum(best))
  
  #add total deaths and log total deaths to the gadm1_sf for mapping and further analysis
  gadm1_udcp_log_sf <- left_join(gadm1_sf, ucdp_adm1_log_sum_df, by="ID_adm1") %>% 
    mutate(deaths1995_1999 = ifelse(is.na(deaths1995_1999), 0, deaths1995_1999),
           log_deaths1995_1999 = ifelse(is.na(log_deaths1995_1999), 0, log_deaths1995_1999)) %>% 
    select(-ISO.y) %>% 
    rename(ISO = ISO.x)

  #clean up interim objects
  rm(missing_adm1_sf)
  rm(missing_sf)
  rm(ucdp_p4_df)
  rm(ucdp_p4_sf)
  
#create a map to visualize what is happening
library(tmap)
tmap_options(check.and.fix = TRUE)

africa_conflict_map <- tm_shape(gadm0_sf) +
  tm_borders(lwd=2) +
  tm_shape(gadm1_udcp_log_sf) +
  tm_polygons(col="deaths1995_1999", title = "1995-1999 conflict deaths") + 
  tm_layout(legend.outside = TRUE,
            main.title = "Conflict deaths 1995-1999 (precision <=ADM1)")

africa_conflict_map
tmap_save(africa_conflict_map,"./figures/africa_conflict_map.png")

africa_log_conflict_map <- tm_shape(gadm0_sf) +
  tm_borders(lwd=2) +
  tm_shape(gadm1_udcp_log_sf) +
  tm_polygons(col="log_deaths1995_1999",title = "ADM1: log(total) 1995-1999 conflict deaths") + 
  tm_layout(legend.outside = TRUE,
            main.title = "1995-1999 Conflict Deaths (log(total) at <=ADM1 precision)")

africa_log_conflict_map

tmap_save(africa_log_conflict_map,"./figures/africa_log_conflict_map.png")

#get DHS points 
dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, lat, lon)

#convert to sf object
dhs_sf <- dhs_df %>% 
  sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

#do a spatial join to get the adm2 id and log deaths for each each dhs point
dhs_deaths_sf <- sf::st_join(dhs_sf, gadm1_udcp_log_sf, join = sf::st_intersects, left=FALSE) %>% 
  select(-ISO,-part_area)
#returns 9586 of 9601 obs

#identify missing rows
missing_ID_adm1 <- anti_join(dhs_sf %>% sf::st_drop_geometry(),
                        dhs_deaths_sf %>% sf::st_drop_geometry(),
                        by="dhs_id") %>% 
  select(dhs_id)
                        
#use a nearest_feature spatial join to look up the rest
missing_sf <- dhs_sf %>% 
  filter(dhs_id %in% missing_ID_adm1$dhs_id)
missing_deaths_sf <- missing_sf %>% 
  sf::st_join(gadm1_udcp_log_sf, join = sf::st_nearest_feature, left=FALSE) 
#found the missing 15; add to the sf object
dhs_deaths_sf <- bind_rows(dhs_deaths_sf,
                           missing_deaths_sf)

#join into the dhs_df, first converting back to df 
dhs_deaths_interim_df <- dhs_deaths_sf %>% 
  sf::st_drop_geometry() %>% 
  select(dhs_id, deaths1995_1999, log_deaths1995_1999, ID_adm1)
#do this to avoid losing the lat/lon column when we coverted to a sf

dhs_deaths_df <- dhs_df %>% 
  left_join(dhs_deaths_interim_df, by="dhs_id")

#check for NAs
# dhs_deaths_df %>% 
#   summarise(across(-dhs_id, ~ any(is.na(.))))

######################################
# Leader Birthplaces
######################################
#get Africa ISO codes
plad_df <- readxl::read_excel("./data/PLAD/PLAD_Oct_2021.xls") %>%
  filter(gid_0 %in% africa_isos_df$iso3 & 
           foreign_leader==0 &
           startyear <= 2014 & 
           endyear >= 2000) %>%
  select(plad_id,leader, startyear, endyear, gid_0, adm0, adm1, adm2, latitude, longitude, geo_precision)
#geo_precision uses same definitions as AidData

#Some of the data lacks ADM2, so use ADM1 level instead
#join with adm1 borders based on lat/long to obtain the border of each ADM1 
# that was a birthplace of any leader in power during these years
plad_sf <- sf::st_as_sf(plad_df, coords = c("longitude", "latitude"), crs = "EPSG:4326") %>% 
            sf::st_transform(plad_sf,crs=sf::st_crs(projection))  %>% 
            sf::st_make_valid(plad_sf)


# Perform spatial join to adm1 level
plad_africa_adm1_sf <- sf::st_join(gadm1_sf, plad_sf, left=FALSE)

#plot to validate
library(tmap)
tmap_options(check.and.fix = TRUE)

plad_map <- tm_shape(gadm0_sf) +
  tm_borders() +
  tm_shape(gadm1_sf) +
  tm_borders() +
  tm_shape(plad_africa_adm1_sf) + 
  tm_fill(col="yellow") +
  tm_layout(main.title.size=1,
            main.title = "ADM1 Birthplaces of leaders in power between 2000 and 2014",
            main.title.position=c("center","top"))
  

plad_map
tmap_save(plad_map,"./figures/Africa_plad_map.png")

#create a binary leader_birthplace variable for each dhs point, giving it a 1 if
#the containing ADM1 was a birthplace of a leader from 2000-2014
dhs_vector_df <- dhs_deaths_df %>%
  mutate(leader_birthplace = ifelse(ID_adm1 %in% plad_africa_adm1_sf$ID_adm1, 1, 0))

dhs_vector_df %>% 
  filter(is.na(leader_birthplace))

write.csv(dhs_vector_df,"./data/interim/dhs_treat_control_vector.csv",row.names=FALSE)  
#dhs_vector_df <-  read.csv("./data/interim/dhs_treat_control_vector.csv") 

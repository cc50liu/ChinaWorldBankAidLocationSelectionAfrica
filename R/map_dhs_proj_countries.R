#map_dhs_proj_countries.R
library(dplyr)
library(sf)
library(tmap)
library(RColorBrewer)

rm(list=ls())

#load data
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

dhs_df <- read.csv("./data/interim/dhs_clusters_id.csv") 

#summarize countries by their DHS status
country_attr <- africa_isos_df %>%
  mutate(dhs_state = case_when(
    iso3 %in% dhs_df$iso3[dhs_df$year < 2000] &
      !(iso3 %in% dhs_df$iso3[dhs_df$year > 2014]) ~ "Pre-2000 only",   
    !(iso3 %in% dhs_df$iso3[dhs_df$year < 2000]) &
      iso3 %in% dhs_df$iso3[dhs_df$year > 2014] ~ "Post-2014 only", 
    iso3 %in% dhs_df$iso3[dhs_df$year < 2000] &
      iso3 %in% dhs_df$iso3[dhs_df$year > 2014] ~ "Both Pre & Post",    
    TRUE ~ "No DHS"
  ))

country_attr$dhs_state <- base::factor(country_attr$dhs_state,
               levels=c("No DHS","Pre-2000 only","Post-2014 only","Both Pre & Post"),
               ordered=T)

#put dot on maps representing number of wb or ch projects there
wb_oda_df <- read.csv("./data/interim/wb_africa_oda_sector_group.csv")
ch_oda_df <- read.csv("./data/interim/ch_africa_oda_sector_group.csv")

wb_oda_df %>% 
  filter(precision_code==1)  %>%
  select(project_title)

#location classes
# 1 Administrative Region
# 2 Populated Place
# 3 Structure 
# 4 Other Topographical Feature
# A tibble: 70 × 3
# Groups:   location_type_code, location_class [70]

# location_type_code location_class     n
# <chr>                       <int> <int>
# 1 ADM3                            1   141
# 2 ADM4                            1    11
# 3 ADMD                            2    14
# 4 ADMF                            3     3
# 5 AIRF                            3     1
# 6 AIRP                            3    41
# 7 AREA                            4    14
# 8 BAY                             4     3
# 9 BDG                             3     3
# 10 BLDG                            3     4
# # … with 60 more rows

wb_oda_df %>% 
  filter(precision_code==2) %>% 
  group_by(location_type_code, location_class) %>% 
  count()

# # A tibble: 36 × 3
# # Groups:   location_type_code, location_class [36]
# location_type_code location_class     n
# <chr>                       <int> <int>
# 1 ADM3                            1    43
# 2 ADM4                            1    13
# 3 AIRP                            3     1
# 4 AREA                            4     4
# 5 BAY                             4     3
# 6 CNL                             3     1
# 7 FLLS                            4     2
# 8 FRM                             3     2
# 9 FRST                            4     3
# 10 HLL                             4     4

wb_oda_df %>% 
  filter(precision_code==1) %>% 
  group_by(ad_sector_codes) %>% 
  count()
#projects cover multiple sectors
# ad_sector_codes         n
# <chr>               <int>
# 1 111|113|112             6
# 2 112|160|220            16
# 3 114                     1
# 4 114|111|113             9
# 5 114|112                 7
# 6 114|113                 7
# 7 114|220                 6
# 8 114|230|240|113        10
# 9 120                     8
# 10 120|111|160|210|140     3

country_attr <- country_attr %>%
  mutate(wb_oda = if_else(iso3 %in% wb_oda_df$site_iso3, TRUE, FALSE),
         ch_oda = if_else(iso3 %in% ch_oda_df$site_iso3, TRUE, FALSE))


#to do: test this after changing shape file
adm0_sf <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
  filter(ISO%in%africa_isos_df$iso3)
st_crs(adm0_sf) = "EPSG:4326"
adm0_sf <- st_transform(adm0_sf,crs=st_crs("ESRI:102023")) 
adm0_sf <- sf::st_make_valid(adm0_sf)

#add country_attr variables to adm0_sf
adm0_sf <- adm0_sf %>% 
  dplyr::left_join(country_attr,by=join_by(shapeGroup==iso3))


dhs_oda_map <- tm_shape(adm0_sf) +
  tm_polygons(col="dhs_state", palette=brewer.pal(4, "YlOrBr"),title="DHS Availability") +
  tm_shape(adm0_sf) +
  tm_borders() +
  tm_shape(adm0_sf[adm0_sf$ch_oda, ]) +
  tm_symbols(size = .5, col = "red", shape = 16, jitter=0.2, labels="China Aid") +
  tm_shape(adm0_sf[adm0_sf$wb_oda, ]) +
  tm_symbols(size = .5, col = "blue", shape = 17, labels="WB Aid") +
  tm_layout(main.title.size=1,
            main.title = paste("Africa DHS Surveys and Aid (2000-2014)"),
            main.title.position=c("center","top")) +
  tm_legend(legend.position = c("right", "bottom"),
            legend.text.size = 1,
            frame = FALSE,
            legend.outside = TRUE, 
            outer.margins = c(0, 0, 0, -0.1), 
            legend.outside.size=.4,
            legend.margin = .05) 

dhs_oda_map

tmap_save(dhs_oda_map,"./figures/Africa_DHS_Aid.png")


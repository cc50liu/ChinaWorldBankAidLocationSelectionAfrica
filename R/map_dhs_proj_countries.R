#map_dhs_proj_countries.R
library(dplyr)
library(sf)
library(tmap)
library(RColorBrewer)

rm(list=ls())

#load data, limit to non-islands
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
africa_map_isos_df <- read.csv("./data/interim/africa_map_isos.csv")

dhs_df <- read.csv("./data/interim/dhs_clusters_id.csv") 

#summarize countries by their DHS status
country_attr <- africa_map_isos_df %>%
  mutate(dhs_state = case_when(
    iso3 %in% dhs_df$iso3[dhs_df$year < 2002] &
      !(iso3 %in% dhs_df$iso3[dhs_df$year > 2013]) ~ "Pre-2002 only",   
    !(iso3 %in% dhs_df$iso3[dhs_df$year < 2000]) &
      iso3 %in% dhs_df$iso3[dhs_df$year > 2013] ~ "Post-2013 only", 
    iso3 %in% dhs_df$iso3[dhs_df$year < 2002] &
      iso3 %in% dhs_df$iso3[dhs_df$year > 2013] ~ "Both Pre & Post",    
    TRUE ~ "No DHS"
  ))

country_attr$dhs_state <- base::factor(country_attr$dhs_state,
               levels=c("No DHS","Pre-2002 only","Post-2013 only","Both Pre & Post"),
               ordered=T)

#put dot on maps for projects 
wb_oda_df <- read.csv("./data/interim/wb_africa_oda_sector_group.csv") %>% 
  filter(site_iso3 %in% africa_map_isos_df$iso3 & 
  transactions_start_year >= 2002 &
  transactions_start_year <= 2013) %>% 
  distinct(site_iso3)
ch_oda_df <- read.csv("./data/interim/ch_africa_oda_sector_group.csv") %>% 
  filter(site_iso3 %in% africa_map_isos_df$iso3 & 
           transactions_start_year >= 2002 &
           transactions_start_year <= 2013) %>% 
  distinct(site_iso3)

country_attr <- country_attr %>%
  mutate(wb_oda = if_else(iso3 %in% wb_oda_df$site_iso3, TRUE, FALSE),
         ch_oda = if_else(iso3 %in% ch_oda_df$site_iso3, TRUE, FALSE))


adm0_sf <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
  filter(ISO%in%africa_map_isos_df$iso3)
st_crs(adm0_sf) = "EPSG:4326"
adm0_sf <- st_transform(adm0_sf,crs=st_crs("ESRI:102023")) 
adm0_sf <- sf::st_make_valid(adm0_sf)

#add country_attr variables to adm0_sf
adm0_sf <- adm0_sf %>% 
  dplyr::left_join(country_attr,by=join_by(ISO==iso3))

dhs_oda_map <- tm_shape(adm0_sf) +
  tm_polygons(col="dhs_state", palette=brewer.pal(4, "Greens"),title="DHS Availability") +
  tm_shape(adm0_sf) +
  tm_borders() +
  tm_shape(adm0_sf[adm0_sf$ch_oda, ]) +
  tm_symbols(size = .5, col = "indianred1", shape = 16, just=c("left","bottom"),
             jitter=.4) +
  tm_shape(adm0_sf[adm0_sf$wb_oda, ]) +
  tm_symbols(size = .5, col = "mediumblue", shape = 17, just=c("right","top")) +
  tm_layout(main.title.size=2,
            main.title = "Africa DHS Surveys and Aid (2002-2013)",
            main.title.position=c("center","top"),
            legend.title.size = 2) +
  tm_add_legend(type = "symbol"
                , shape=c(16,17)
                , col = c("indianred1","mediumblue")
                , labels = c("China Aid","WB Aid"))  +
  tm_legend(legend.position = c("left", "bottom"),
            legend.text.size = 1.7,
            legend.width = -1,
            frame = F,
            legend.outside = F, 
            outer.margins = c(0, 0, 0, 0)) 

dhs_oda_map

tmap_save(dhs_oda_map,"./figures/Africa_DHS_Aid.png")


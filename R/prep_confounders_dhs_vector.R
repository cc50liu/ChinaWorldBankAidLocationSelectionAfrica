#prep_confounders_dhs_vector.R
#uses sf libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(tmap)
library(purrr)
rm(list=ls())

################################################
### Load administrative borders and general data

#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
projection <- "ESRI:102023"

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

#settings to generate maps
tmap_options(check.and.fix = TRUE)
africa_map_isos_df <- read.csv("./data/interim/africa_map_isos.csv")

################################################
# Conflict Deaths, limit to precise-enough for subnational use
ucdp_p4_sf <- read.csv("./data/UCDP/GEDEvent_v23_1.csv") %>% 
  filter(region=="Africa" &
         year %in% 1997:2013 & 
         where_prec <= 4) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>%
  select(id, best, year, where_prec, adm_1, adm_2, geometry) %>%   
  sf::st_transform(crs=sf::st_crs(projection))  


  #Do a spatial join to identify adm1 of all obs
  #this keeps the geometry of the ucdp point data
  ucdp_p4_adm1_sf <- ucdp_p4_sf %>% 
    sf::st_join(gadm1_sf, join = sf::st_intersects, left=FALSE) 
  #returns 18892 of 18942 obs
  
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
    summarize(
      deaths1997_1999 = sum(best[year >= 1997 & year <= 1999]),
      deaths1998_2000 = sum(best[year >= 1998 & year <= 2000]),
      deaths1999_2001 = sum(best[year >= 1999 & year <= 2001]),
      deaths2000_2002 = sum(best[year >= 2000 & year <= 2002]),
      deaths2001_2003 = sum(best[year >= 2001 & year <= 2003]),
      deaths2002_2004 = sum(best[year >= 2002 & year <= 2004]),
      deaths2003_2005 = sum(best[year >= 2003 & year <= 2005]),
      deaths2004_2006 = sum(best[year >= 2004 & year <= 2006]),
      deaths2005_2007 = sum(best[year >= 2005 & year <= 2007]),
      deaths2006_2008 = sum(best[year >= 2006 & year <= 2008]),
      deaths2007_2009 = sum(best[year >= 2007 & year <= 2009]),
      deaths2008_2010 = sum(best[year >= 2008 & year <= 2010]),
      deaths2009_2011 = sum(best[year >= 2009 & year <= 2011]),
      deaths2010_2012 = sum(best[year >= 2010 & year <= 2012]),
      deaths2011_2013 = sum(best[year >= 2011 & year <= 2013]),
    ) %>%
    ungroup()
  
  #clean up interim objects
  rm(missing_adm1_sf)
  rm(missing_sf)
  rm(ucdp_p4_sf)
  rm(ucdp_p4_adm1_sf)
  rm(ucdp_p4_adm1_df)
  
  #create a density plot to look at the distribution of the values
  udcp_density <- ucdp_adm1_sum_df %>% 
    pivot_longer(starts_with("deaths"), names_to = "death_years", values_to = "deaths") %>%
    ggplot(aes(deaths, color=death_years)) +
    geom_density() +
    labs(x = "Total Deaths (prec<=4)", y = "Density across ADM1s",
         title="Conflict Deaths across ADM1s",color="Year")  +
    scale_color_discrete(labels = function(x) gsub(".*?(\\d{4}_\\d{4})$", "\\1", x)) +
    theme_bw()
  
  #highy right skewed - use the log
  ggsave("./figures/udcp_density.png",udcp_density, width=6, height = 10, dpi=300,
      bg="white", units="in")
  rm(udcp_density)

  #create logged versions of variables, adding 1 to avoid taking log of 0
  ucdp_adm1_log_sum_df <- ucdp_adm1_sum_df %>%
    mutate(
      across(starts_with("deaths"), ~ log(. + 1), .names = "log_{.col}")
    )
  
  # Generate density plots for each logged variable
  udcp_log_density <- ucdp_adm1_log_sum_df %>%
    pivot_longer(starts_with("log_deaths"), names_to = "death_years", values_to = "log_deaths") %>%
    ggplot(aes(log_deaths, color=death_years)) +
    geom_density() +
    labs(x = "Log Total Deaths (prec<=ADM1)", y = "Density across ADM1s",
         title="Conflict Deaths (log) across ADM1s",color="3-year sum")  +
    scale_color_discrete(labels = function(x) gsub(".*?(\\d{4}_\\d{4})$", "\\1", x)) +
    theme_bw()
  
  ggsave("./figures/udcp_log_density.png", udcp_log_density, width = 6, height = 6, dpi = 300, bg = "white", units = "in")
  rm(udcp_log_density)
  rm(ucdp_adm1_sum_df)
  
  #add total deaths and log total deaths to the gadm1_sf for mapping and further analysis
  gadm1_udcp_log_sf <- left_join(gadm1_sf, ucdp_adm1_log_sum_df, by="ID_adm1") %>% 
    mutate(across(starts_with("deaths"), ~ ifelse(is.na(.), 0, .))) %>%
    mutate(across(starts_with("log_deaths"), ~ ifelse(is.na(.), 0, .))) %>%
    select(-ISO.y) %>% 
    rename(ISO = ISO.x)

  #create maps to visualize what is happening
  # Get the list of "log_deaths..." column names
  log_deaths_columns <- names(ucdp_adm1_log_sum_df)[grepl("^log_deaths", names(ucdp_adm1_log_sum_df))]
  
  #subset the data to exclude islands for mapping
  gadm0_map_sf <- gadm0_sf[gadm0_sf$ISO %in% africa_map_isos_df$iso3, ]
  gadm1_udcp_log_map_sf <- gadm1_udcp_log_sf[gadm1_udcp_log_sf$ISO %in% africa_map_isos_df$iso3, ]
  
  # Loop through each "log_deaths..." column and create a map of Conflict Deaths
  for (column in log_deaths_columns) {
    #column <- "log_deaths1997_1999"  #uncomment to test
    map_years <- sub("^log_deaths", "", column)
    map_years <- sub("_", "-", map_years)
    map_title <- paste("Conflict Deaths",map_years,"(<=ADM1 precision)")
    
    africa_log_conflict_map <- tm_shape(gadm0_map_sf) +
      tm_borders(lwd = 2) +
      tm_shape(gadm1_udcp_log_map_sf) +
      tm_polygons(col = column, title = paste(map_years,"Total(log)")) +
      tm_layout(legend.outside = F, 
                legend.position = c("left", "bottom"),
                main.title = map_title)
    
    tmap_save(africa_log_conflict_map, paste0("./figures/africa_conflict_map_", column, ".png"))
  }
  #cleanup interim variables
  rm(africa_log_conflict_map)
  rm(log_deaths_columns)
  rm(gadm1_udcp_log_map_sf)

  #########
  #get DHS points 
  dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
    select(dhs_id, lat, lon)
  
  #convert to sf object
  dhs_sf <- dhs_df %>% 
    sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
    sf::st_transform(crs=sf::st_crs(projection))
  
  #do a spatial join to get the adm1 id and log deaths for each each dhs point
  dhs_deaths_sf <- sf::st_join(dhs_sf, gadm1_udcp_log_sf, join = sf::st_intersects, left=FALSE) %>% 
    select(-ISO,-part_area)

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
    select(dhs_id, ID_adm1, starts_with("deaths"), starts_with("log_"))
  #do this to avoid losing the lat/lon column when we coverted to a sf
  
  dhs_deaths_df <- dhs_df %>% 
    left_join(dhs_deaths_interim_df, by="dhs_id")
  
  #check for NAs
  # dhs_deaths_df %>%
  #   summarise(across(-dhs_id, ~ any(is.na(.))))
  
  #remove interim objects
  rm(dhs_deaths_interim_df)
  rm(dhs_deaths_sf)
  rm(missing_deaths_sf)
  rm(missing_ID_adm1)
  rm(missing_sf)
  rm(gadm1_udcp_log_sf)
  rm(ucdp_adm1_log_sum_df)


######################################
# Leader Birthplaces
######################################
plad_df <- readxl::read_excel("./data/PLAD/PLAD_Oct_2021.xls") %>%
  filter(gid_0 %in% africa_isos_df$iso3 & 
           foreign_leader==0 &
           startyear <= 2013 & 
           endyear >= 1999) %>%
  select(plad_id,leader, startyear, endyear, gid_0, adm0, adm1, adm2, latitude, longitude, geo_precision)
  
#create a longer version with a row for each year each leader was in power 
plad_long_df <- plad_df %>% 
  mutate(year= map2(startyear, endyear, ~seq(from = .x, to = .y, by = 1))) %>%
  unnest(year) %>% 
  filter(year %in% 1999:2013)
    

#convert to sf object
plad_sf <- sf::st_as_sf(plad_long_df, coords = c("longitude", "latitude"), crs = "EPSG:4326") %>% 
            sf::st_transform(crs=sf::st_crs(projection))  %>% 
            sf::st_make_valid()

#Some of the data lacks ADM2, so use ADM1 level instead
plad_africa_adm1_sf <- sf::st_join(gadm1_sf, plad_sf, left=FALSE)

#plot to validate
gadm1_map_sf <- gadm1_sf[gadm1_sf$ISO %in% africa_map_isos_df$iso3, ]
plad_africa_adm1_map_sf <- plad_africa_adm1_sf[plad_africa_adm1_sf$ISO %in% africa_map_isos_df$iso3, ]

# Loop through each year to create a map of Leader Birthplaces
for (year in 1999:2013) {
  #year <- 1999  #uncomment to test
  map_title <- paste("Birthplaces (ADM1) of leaders in power in",year)
  
  plad_map <- tm_shape(gadm0_map_sf) +
    tm_borders() +
    tm_shape(gadm1_map_sf) +
    tm_borders() +
    tm_shape(plad_africa_adm1_sf[plad_africa_adm1_sf$year==year,]) + 
    tm_fill(col="yellow") +
    tm_layout(main.title.size=1,
              main.title = map_title,
              main.title.position=c("center","top"))
  
  tmap_save(plad_map, paste0("./figures/africa_plad_map_", year, ".png"))
}
#cleanup interim variables
rm(plad_map)
rm(plad_africa_adm1_map_sf)

#create binary leader_YYYY_birthplace variables for each dhs point
# dhs_vector_df <- dhs_deaths_df %>%
#   mutate(leader_birthplace = ifelse(ID_adm1 %in% plad_africa_adm1_sf$ID_adm1, 1, 0))

#convert to df and create binary leader_YYYY variables
plad_africa_adm1_df <- plad_africa_adm1_sf %>% 
  sf::st_drop_geometry() %>% 
  select(ID_adm1, year) %>% 
  distinct() %>% 
  arrange(year) %>% 
  pivot_wider(names_from=year, values_from=year,
              values_fn = function(x) ifelse(!is.na(x), 1, 0), 
              names_prefix="leader_",
              values_fill=0)

#join to dhs points based on ID_adm1 field
dhs_vector_df <- dhs_deaths_df %>%
  left_join(plad_africa_adm1_df, by = "ID_adm1") %>% 
  mutate(across(starts_with("leader"), ~ ifelse(is.na(.),0,.)))

write.csv(dhs_vector_df,"./data/interim/dhs_treat_control_vector.csv",row.names=FALSE)  
#dhs_vector_df <-  read.csv("./data/interim/dhs_treat_control_vector.csv") 

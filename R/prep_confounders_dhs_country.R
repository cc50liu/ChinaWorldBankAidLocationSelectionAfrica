#prep_confounders_dhs_country.R
library(dplyr)
library(tidyr)
#install.packages("countrycode")
library(countrycode)
rm(list=ls())


################################################
### Load general data
#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
projection <- "ESRI:102023"

#boundaries used in Gehring
# gadm0_sf <- sf::st_read("./data/country_regions/gadm28_adm0.shp")  %>%
#   filter(ISO %in% africa_isos_df$iso3)
# sf::st_crs(gadm0_sf) = "EPSG:4326"
# gadm0_sf <- sf::st_transform(gadm0_sf,crs=sf::st_crs(projection))
# gadm0_sf <- sf::st_make_valid(gadm0_sf)
# unique(sf::st_is_valid(gadm0_sf))

################################################
### get DHS points 
dhs_df <- read.csv("./data/interim/dhs_treat_control_raster.csv") %>% 
  select(dhs_id, lat, lon, country, iso3)
  
#convert to sf object
dhs_sf <- dhs_df %>% 
  sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  sf::st_transform(crs=sf::st_crs(projection))

################################################
### Load Polity V data
polity_df <- readxl::read_excel("./data/PolityV/p5v2018.xls") %>% 
  filter(year %in% 1999:2013) %>% 
  mutate(iso3=countrycode(sourcevar=country,origin="country.name",destination="iso3c")) %>% 
  filter(iso3 %in% africa_isos_df$iso3) %>% 
  select(country, iso3, year, polity2) 

#remove extra Sudan 2011 record that collides with south/north specific measures
#using the same iso code, record 7 below
polity_df %>% 
  filter(year %in% 2011:2013 & iso3 %in% c("SDN","SSD"))
# 1 Sudan-North SDN    2011      -4
# 2 Sudan-North SDN    2012      -4
# 3 Sudan-North SDN    2013      -4
# 4 South Sudan SSD    2011       0
# 5 South Sudan SSD    2012       0
# 6 South Sudan SSD    2013       0
# 7 Sudan       SDN    2011      -2

polity_df <- polity_df %>% 
  filter(!(year==2011 & country=="Sudan-North")) 

################################################
### GDP per capita and GINI coefficient of inequality
wdi_df <- readxl::read_excel("./data/WorldDevelopmentIndicators/P_Data_Extract_From_World_Development_Indicators.xlsx",
                             .name_repair="universal") %>% 
  filter(Country.Code %in% africa_isos_df$iso3) %>% 
  rename(country="Country.Name",
         iso3="Country.Code")

#reshape to have year records for each variable
wdi_long_df <- wdi_df %>% 
  pivot_longer(cols=starts_with(".."), names_to="year", values_to="value",
               names_pattern="\\.{2}(\\d+)\\..+") %>% 
  mutate(year=as.integer(year),
         Series.Code=case_match(Series.Code,
                                "NY.GDP.PCAP.KD" ~ "gdp_per_cap_USD2015",
                                "SI.POV.GINI" ~ "country_gini"
                                )) %>% 
  select(-Series.Name) %>% 
  pivot_wider(names_from = Series.Code, values_from = value)
       

#exclude countries that don't have polity scores nor dhs points
wdi_long_df <-  wdi_long_df %>% 
  filter(!iso3 %in% c("STP", "SYC", "SSD")) 

#join the two datasets, using country name from wdi_long_df
country_confounder_df <- wdi_long_df %>% 
  left_join(polity_df[,!(names(polity_df) %in% "country")], by=join_by(iso3,year))

#exclude countries that don't have a country_gini measure nor dhs
country_confounder_df <- country_confounder_df %>% 
  filter(!iso3 %in% c("GNQ","ERI","LBY","SOM"))

#fill in missing year gini's with the most recent year's value
#or, for early years, set to first known value
country_confounder_gini_fill_df <- country_confounder_df %>%
  group_by(iso3) %>%
  mutate(
    country_gini = if_else(
      country_gini == ".." | is.na(country_gini),
      NA_character_, # Replace ".." or NA values with NA
      country_gini
    )
  ) %>%
  tidyr::fill(country_gini, .direction = "downup") %>%
  ungroup()

#check results
# country_confounder_gini_fill_df %>% 
#   filter(iso3=="ZMB")
# 
# country_confounder_df %>% 
#   filter(iso3=="ZMB")


#fill in missing gdp_per_cap_USD2015 values using same approach
country_confounder_gdp_fill_df <- country_confounder_gini_fill_df %>%
  group_by(iso3) %>%
  mutate(
    gdp_per_cap_USD2015 = if_else(
      gdp_per_cap_USD2015 == ".." | is.na(gdp_per_cap_USD2015),
      NA_character_, # Replace ".." or NA values with NA
      gdp_per_cap_USD2015
    )
  ) %>%
  tidyr::fill(gdp_per_cap_USD2015, .direction = "downup") %>%
  ungroup()

#verify results
# country_confounder_gini_fill_df %>%
#   filter(iso3=="DJI")
# 
# country_confounder_gdp_fill_df %>%
#   filter(iso3=="DJI")

#convert gdp and gini scores to numbers, and scale country_gini
country_confounder_complete_df <- country_confounder_gdp_fill_df %>% 
  mutate(gdp_per_cap_USD2015 = as.double(gdp_per_cap_USD2015),
         country_gini = as.double(country_gini) / 100)

write.csv(country_confounder_complete_df,"./data/interim/country_confounders.csv",row.names=FALSE)  

# prep_confounder_disasters.R
# Subset the GDIS file to disasters in Africa between 2001 and 2014, using an excel file of id's from EM-DAT
library(readxl)
library(sf)
library(dplyr)

rm(list=ls())

# Read an EM-DAT excel file that has a subset of disasters that happened in Africa between 2001 and 2014
af_disasters_df <- read_excel("./data/EM-DAT/public_emdat_custom_request_2024-03-07_aa95ca9d-5402-497c-b0c5-e0ca2e57cf56.xlsx",
                           sheet = "EM-DAT Data") %>% 
  mutate(match_id = sub("^(.*?-.*)-.*$", "\\1", DisNo.))
#2582 obs. Includes technological and biological disasters that haven't been geocoded

# Read GDIS Geodatabase file of vector objects 
disaster_sf <- st_read(dsn = "./data/GDIS/pend-gdis-1960-2018-disasterlocations.gdb")
#Simple feature collection with 39953 features and 17 fields

# Subset vector objects based on the IDs from the Excel file
af_disaster_sf <- disaster_sf[disaster_sf$disasterno %in% af_disasters$match_id, ]
#leaves 3395 obs

# Add in the rest of the columns in the Excel file to the sf object
af_disaster_attr_sf <- inner_join(af_disaster_sf, af_disasters_df, 
                                  by=join_by(disasterno==match_id),
                                  relationship = "many-to-many") %>% 
  rename(longLocation=Location) %>% 
  select(-country)
                                  
af_disaster_attr_sf %>% 
  select(disasterno, Country, location, Location) %>% 
  filter(location!=Location)

#Save subsetted sf file with additional columns for use later
st_write(af_disaster_attr_sf, "./data/interim/af_disasters.geojson", driver = "GeoJSON")

sort(names(af_disaster_attr_sf))


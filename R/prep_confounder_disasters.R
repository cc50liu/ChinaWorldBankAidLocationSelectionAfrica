# prep_confounder_disasters.R
# Subset the GDIS file to disasters in Africa between 2001 and 2014, using an excel file of id's from EM-DAT
library(readxl)
library(sf)
library(dplyr)

rm(list=ls())

# Read an EM-DAT excel file that has a subset of disasters that happened in Africa between 2019 and 2014
af_disasters_df <- read_excel("./data/EM-DAT/africa_1999_to_2014.xlsx",
                           sheet = "EM-DAT Data") 
#3028 obs. Includes technological and biological disasters that haven't been geocoded

# Read GDIS Geodatabase file of vector objects:  39953 obs
disaster_sf <- st_read(dsn = "./data/GDIS/pend-gdis-1960-2018-disasterlocations.gdb") %>% 
  #add a match id including the 3 digit ISO code added by EM-DAT in the Excel file
  mutate(match_id = paste(disasterno,iso3,sep = "-"))
#Simple feature collection with 39953 features and 17 fields

# Inner join to excel file to subset to rows there and add excel columns - leaves 3028 obs 
af_disaster_attr_sf <- inner_join(disaster_sf, af_disasters_df, 
                                  by=join_by(match_id==DisNo.)) %>% 
  rename(CapCountry=Country,
         CapLocation=Location)

# af_disaster_attr_sf %>% 
#   filter(disasterno=="2009-0092")
# 
# af_disaster_attr_sf %>% 
#   select(disasterno, DisNo., iso3, ISO, Country)
# 
# af_disaster_attr_sf %>% 
#   select(country,CapCountry,match_id) %>% 
#   filter(country!=CapCountry) %>% 
#   print(n=100)
# 
# af_disaster_attr_sf %>% 
#   select(location,CapLocation,match_id) %>% 
#   filter(location!=CapLocation) %>% 
#   print(n=100)

#Save subsetted sf file with additional columns for use in later script
st_write(af_disaster_attr_sf, "./data/interim/af_disasters.geojson", driver = "GeoJSON")
#af_disaster_attr_sf <- st_read("./data/interim/af_disasters.geojson")



# prep_confounder_disasters.R
# Subset the GDIS file to disasters in Africa between 2001 and 2014, using an excel file of id's from EM-DAT
library(readxl)
library(sf)

# Read an EM-DAT excel file that has a subset of disasters that happened in Africa between 2001 and 2014
af_disasters <- read_excel("./data/EM-DAT/public_emdat_custom_request_2024-03-07_aa95ca9d-5402-497c-b0c5-e0ca2e57cf56.xlsx",
                           sheet = "EM-DAT Data")  

# Read GDIS Geodatabase file of vector objects 
disaster_sf <- st_read(dsn = "./data/GDIS/pend-gdis-1960-2018-disasterlocations.gdb")

# Subset vector objects based on the IDs from the Excel file
af_disaster_sf <- disaster_sf[disaster_sf$disasterno %in% af_disasters$DisNo., ]

# Print the subset data
print(af_disaster_sf)

#Save smaller sf file for use later
st_write(af_disaster_sf, "./data/interim/af_disasters.sf")
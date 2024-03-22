#prep_iso_codes.R
library(dplyr)

rm(list=ls())
################################################################################
# Get list of Africa ISO country codes                          
################################################################################
africa_isos_df <- read.csv("./data/all_iso_codes.csv",na.strings="") %>%
  #use na.strings so NA of Namibia isn't interpreted as not available
 filter(region=="Africa") %>%
 select(name, alpha.3, alpha.2) %>%
 rename(iso3=alpha.3,
        iso2=alpha.2)

#write it for use in other scripts
write.csv(africa_isos_df,"./data/interim/africa_isos.csv",row.names = FALSE)
#africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

#create a version without small islands for use in maps
africa_map_isos_df <- africa_isos_df %>% 
  filter(!iso3 %in% c("ATF","CPV", "COM","IOT","MUS","REU", "SHN", "STP", "SYC"))
# ATF French Southern Territories
# CPV Cape Verde
# COM Comoros
# IOT British Indian Ocean Territory   
# MUS Mauritius
# REU Reunion
# SHN Saint Helena
# STP Sao Tome and Principe
# SYC Seychelles

#write it for use in other scripts
write.csv(africa_map_isos_df,"./data/interim/africa_map_isos.csv",row.names = FALSE)

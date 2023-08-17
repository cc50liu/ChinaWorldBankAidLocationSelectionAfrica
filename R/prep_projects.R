################################################################################
# Load China and WB project data; filter to aid projects in Africa 2001-2014;
# data cleanup and preparation
################################################################################
library(dplyr)
library(sf)

rm(list=ls())
################################################################################
# Get list of Africa ISO country codes                          
################################################################################
africa_isos_df <- read.csv("./data/all.csv",na.strings="") %>%
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

########################################################################
####  World Bank                                                    ####
########################################################################
wb_proj <- read.csv("./data/AiddataWB1.4.2/projects.csv")
wb_locs <- read.csv("./data/AiddataWB1.4.2/locations.csv")
wb_geocoded <-  inner_join(wb_proj, wb_locs, by="project_id",multiple = "all")

wb_proj_ancillary <- read.csv("./data/AiddataWB1.4.2/projects_ancillary.csv") %>%
  filter(REGION=="AFRICA")

#narrow to projects in Africa and study context
wb_africa_oda_p4_df <- semi_join(wb_geocoded, wb_proj_ancillary, by=c("project_id"="PROJECT.ID")) %>%
  filter(transactions_start_year > 2000 )
#do not yet limit by precision or end date here; will do so later

#remove the objects that were used to construct wb_africa_oda_p4_df
rm(wb_proj)
rm(wb_locs)
rm(wb_geocoded)
rm(wb_proj_ancillary)

#Rows with "Unspecified" recipients_iso3 are spread across multiple locations.  Use their gazetteer_adm_code
#to construct new site_iso3 and site_iso2 columns for country of project_locations
#from data dictionary:  gazetteer_adm_code is the hierarchical order of place, 
#represented by geoname IDs, associated with the place name's geoname_id. Hierarchy is pipe delimited
#example:  6295630|6255146|KE|28|7844635
# More info about "Unspecified" projects: will use site_iso3 when needed in analysis
#https://github.com/orgs/AIandGlobalDevelopmentLab/projects/6/views/1?filterQuery=Unspecified&pane=issue&itemId=29677314


# wb_africa_oda_p4_df %>%
#   filter(recipients_iso3=="Unspecified") %>%
#   select(gazetteer_adm_code, project_title)
wb_africa_oda_df <- wb_africa_oda_p4_df %>%
  mutate(site_iso2 = gsub(pattern=".*\\|([A-Z]{2})\\|.*","\\1", x=gazetteer_adm_code)) %>%
  left_join(africa_isos_df, by=c("site_iso2" = "iso2")) %>%
  rename(site_iso3 = iso3) %>% 
  select(-name)

#look at rows where recipients and site iso3 don't match.  
# wb_africa_oda_df %>%
#   distinct(recipients_iso3, site_iso3) %>%
#   filter(ifelse(is.na(recipients_iso3),"-2",recipients_iso3) != ifelse(is.na(site_iso3),"-1",site_iso3))

# - Unspecified to something else is correct
# - <NA> investigated and corrected below
# - TZA to KEN.   TZA may have received the money - leave as is
# #Central Transport Corridor Project that entered Kenya.  We care about the site location
# #more than funding location
# #P078387_7844635                  6295630|6255146|KE|28|7844635             TZA       KEN
# wb_africa_oda_df %>% 
#   filter(project_location_id=="P078387_7844635") %>% 
#   select(project_title, ad_sector_codes, place_name) 
# # project_title ad_sector_codes place_name
# # 1 Central Transport Corridor Project         151|210  Horo Horo
# 
# #I also did a spatial join but that did not add any new info.  (see deleted_code.txt)

#investigate one row with NA in site_iso3 - all data suggests should be same as recipients_iso3
wb_africa_oda_df %>% 
  select(project_id,project_location_id,recipients_iso3, site_iso3,gazetteer_adm_code,project_title, precision_code,place_name, 
         ad_sector_codes,place_name, latitude,longitude,location_type_code,location_type_name,
         location_class,geographic_exactness) %>% 
  filter(is.na(site_iso3))
#update to use recipients_iso3 for this one record
wb_africa_oda_df <- wb_africa_oda_df %>% 
  mutate(site_iso3 = if_else(project_location_id == "P117764_10344471", recipients_iso3, site_iso3))


# write a file of the projects whose site_iso3 doesn't match recipients_iso3
wb_africa_oda_df %>%
  filter(recipients_iso3 != site_iso3) %>%
  write.csv("./data/interim/wb_iso3_mismatch.csv", row.names=FALSE)

# #generate a list of distinct iso codes
# wb_isos <- wb_africa_oda_df %>% 
#   distinct(site_iso3) 
# 
# #write individual files for each country's oda
# o <- lapply(wb_isos$site_iso3, function(iso) {
#   filtered_df <- wb_africa_oda_df %>% 
#     filter(site_iso3 == iso)
#   write.csv(filtered_df, file = paste0("./data/interim/wb_",tolower(iso), "_oda_p4.csv"), row.names = FALSE)
# })

#remove wb-related objects if we successfully wrote to files
# rm(o)
# rm(wb_isos)
rm(wb_africa_oda_p4_df)

#create separate rows for each sector
#read OEDC sector list to map to parent codes; adjustments to match China data
oedc_sect_df <- readxl::read_excel('./data/OECD/DevFi_Classification.xlsx') %>% 
  rename(parent_code = "Parent code",
         code_name_e = "code name e",
         sect_code = code) %>% 
  mutate(sect_code = as.character(sect_code),
         #replace unicode no-break space with NA
         parent_code = ifelse(parent_code=="\u00a0",NA,parent_code), 
         #give some subsectors parents to match how China data is grouped
         parent_code = case_match(sect_code,
                                "311" ~ "310",  #Agriculture => Agriculture, Forestry, Fishing
                                "312" ~ "310",  #Forestry => Agriculture, Forestry, Fishing
                                "321" ~ "320",  #Industry => Industry, Mining, Construction 
                                "322" ~ "320",  #Mineral Resources and Mining => Industry, Mining, Construction 
                                "331" ~ "330",  #Trade Policies and Regulations => Trade and Tourism
                                 .default = parent_code),
         #remove leading roman numerals from sect_code_name
         sect_code_name = gsub(pattern="^.*\\. (.*)",
                                replacement="\\1",
                                x=code_name_e)) %>% 
  select(sect_code, sect_code_name, parent_code) %>% 
  #add 330 Trade and Tourism which is used in Chinese data but isn't here
  add_row(sect_code="330",sect_code_name="Trade and Tourism",parent_code=NA)

#create separate project rows for each sector, replace sub-sectors with parent sectors,
#remove duplicate rows caused by multiple sub-sectors rolling up to one parent
wb_sect_df <- wb_africa_oda_df %>% 
  tidyr::separate_rows(c(ad_sector_codes), sep="\\|") %>% 
  left_join(oedc_sect_df, by=join_by(ad_sector_codes==sect_code)) %>% 
  mutate(ad_sector_top = ifelse(is.na(parent_code), ad_sector_codes, parent_code)) %>% 
  select(-ad_sector_codes,-sect_code_name,-parent_code) %>%
  distinct() %>%
  #join again to get the sect_code name and rename to have same column names as china
  left_join(oedc_sect_df, by=join_by(ad_sector_top==sect_code)) %>%
  select(-parent_code, -ad_sector_names)  %>% 
  rename(ad_sector_codes = ad_sector_top,
         ad_sector_names = sect_code_name)

#add sector group columns
wb_sect_group_df <- wb_sect_df %>% 
  mutate(sector_group = case_match(as.double(ad_sector_codes),
                         #Social Infrastructure & Services
                                  110 ~ "SIS",  #Education
                                  120 ~ "SIS",  #Health
                                  130 ~ "SIS",  # Population Policies / Programmes and Reproductive Health
                                  140 ~ "SIS",  # Water Supply and Sanitation
                                  150 ~ "SIS",  #Government and Civil Society
                                  160 ~ "SIS",  #Other Social Infrastructure and Services
                         #Economic Infrastructure & Services 
                                  210 ~ "EIS",  #Transport and Storage
                                  220 ~ "EIS",  # Communications
                                  230 ~ "EIS",  # Energy Generation and Supply
                                  240 ~ "EIS",  # Banking and Financial Services
                                  250 ~ "EIS",  # Business and Other Services
                         # Production     
                                  310 ~ "PRO",  #Agriculture, Forestry and Fishing
                                  320 ~ "PRO",  #Industry, Mining, Construction
                                  330 ~ "PRO"  #Trade and Tourism
                         #Leave others NA
         ),
         sector_group_name = case_match(sector_group,
                                        "SIS" ~ "Social Infrastructure & Services",
                                        "EIS" ~ "Economic Infrastructure & Services",
                                        "PRO" ~ "Production"))


#update sector names to be consistent between WB and China
wb_sect_group_df <- wb_sect_group_df %>% 
  mutate(ad_sector_names=case_match(ad_sector_names,
                                    "Other Social infrastructure and services" ~ "Other Social Infrastructure and Services",
                                    "General Environment Protection" ~ "General Environmental Protection",
                                    "Energy" ~ "Energy Generation and Supply",
                                    "Agriculture, Forestry, Fishing" ~ "Agriculture, Forestry and Fishing",
                                    .default = ad_sector_names)
  )

write.csv(wb_sect_group_df,"./data/interim/wb_africa_oda_sector_group.csv",row.names = FALSE)
#wb_sect_group_df <- read.csv("./data/interim/wb_africa_oda_sector_group.csv")


################################################################################
# China Project Data (AidData 1.1.1)
################################################################################ 
ch_oda_df <- read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oda-like_flows.csv") 

#limit to projects with location details and study years
ch_oda_p4_df <- ch_oda_df %>%
         #include all precisions for desc stats; filtered to <=3 in select_dhs_year_treat_control_sectorAfrica
         #filter(precision_code<=4 &  # 1=exact, 2=up to 25km, 3=adm2, or 4=adm1
         #status %in% c("Completion","Implementation") & #dataset already limits to this
         #flow_class=="ODA-like" &  #dataset already limits to this
         filter(umbrella==FALSE &    
         transactions_start_year <= 2014 &
         transactions_start_year >= 2001 &
         !is.na(latitude)) 

# some recipients_iso3 have problematic entries, resolved in code below
# 1              VNM|KHM|MMR
# 2         Africa, regional
# 3     Africa, regional|ZWE
# 4     Africa, regional|KEN
# 5     SEN|Africa, regional
# 6 TZA|ZMB|Africa, regional

#exclude records where recipients_iso3 has (1) a single non-Africa iso3 code
#or (2) "VNM|KHM|MMR"
non_africa_isos_df <- read.csv("./data/all.csv") %>%
  filter(region!="Africa") %>%
  select(name, alpha.3, alpha.2) %>%
  rename(iso3=alpha.3,
         iso2=alpha.2)

ch_oda_africa_p4_df <- ch_oda_p4_df %>%
  filter(!recipients_iso3 %in% non_africa_isos_df$iso3 &
         !recipients_iso3=="VNM|KHM|MMR")

#clean up non-Africa datasets
rm(ch_oda_df)
rm(ch_oda_p4_df)
rm(non_africa_isos_df)

#create site_iso2 and site_iso3 codes based on the gazetteer_adm_code
ch_oda_africa_site_df <- ch_oda_africa_p4_df %>%
  mutate(site_iso2 = gsub(pattern=".*\\|([A-Z]{2})\\|.*","\\1", x=gazetteer_adm_code)) %>%
  left_join(africa_isos_df, by=c("site_iso2" = "iso2")) %>%
  rename(site_iso3 = iso3) %>% 
  select(-name) 

#review differences
#make sure "regional" rows look appropriate.  looks good
# ch_oda_africa_site_df %>% 
#   filter(grepl("regional",recipient_iso3)) %>% 
#   select(recipient_iso3, site_iso3, gazetteer_adm_code)

#now exclude "regional" rows to identify anomalies; 28 of them
ch_oda_africa_site_df %>% 
  filter(ifelse(is.na(site_iso3),-1,site_iso3) != ifelse(is.na(recipients_iso3),-1,recipients_iso3) &
           !(grepl("regional",recipient_iso3))) %>% 
  select(project_location_id, gazetteer_adm_code, recipients_iso3, site_iso3) 

#remove two records whose gazetteer locations put them in Costa Rica and Brazil
ch_oda_africa_site_cleaned_df <- ch_oda_africa_site_df %>% 
  filter(!project_location_id %in% c("31253_3399415","1020_3623076"))

#difference between Sudan and South Sudan not a problem due to creation of SSD in 2011
#- site_iso3 contains the country that exists today 
#project_location_id == "35176_373303"

#the other records are all still within Africa, join to ADM0 shapefile to help 
#deal with anomalies
projection <- "ESRI:102023"
country_borders <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(country_borders) = "EPSG:4326"
country_borders <- sf::st_transform(country_borders,crs=sf::st_crs(projection)) 
country_borders <- sf::st_make_valid(country_borders)

ch_oda_africa_site_sf <- ch_oda_africa_site_cleaned_df %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
  sf::st_transform(crs=st_crs(projection))
ch_oda_africa_site_sf <- sf::st_make_valid(ch_oda_africa_site_sf)

ch_oda_africa_site_intersect_adm0_sf <- sf::st_join(ch_oda_africa_site_sf, country_borders,
                                          join=st_intersects) 

#can geometry help us identify correct values for differences?
#evaluate records where spatial ISO differs from site (gazetteer based) iso
ch_oda_africa_site_intersect_adm0_sf %>% 
  sf::st_drop_geometry() %>% 
  filter(ifelse(is.na(site_iso3),-1,site_iso3) != ifelse(is.na(ISO),-1,ISO) &
         project_location_id != "35176_373303") %>% 
  select(project_location_id, project_title, gazetteer_adm_code, recipients_iso3, site_iso3, ISO)

#in two cases, gazetteer data doesn't match the recipient_iso or spatial join
#update site_iso3 to match recipient_iso and spatial join value
ch_oda_africa_site_cleaned1_df <- ch_oda_africa_site_cleaned_df %>% 
  mutate(site_iso3 = if_else(project_location_id %in% c("30315_3346401","35629_11592387"),
                             recipients_iso3, site_iso3))
ch_oda_africa_site_intersect_adm0_sf <- ch_oda_africa_site_intersect_adm0_sf %>% 
  mutate(site_iso3 = if_else(project_location_id %in% c("30315_3346401","35629_11592387"),
                             recipients_iso3, site_iso3))

#for cases where ISO is null, try a nearest feature join
missing_adm0_sf <- ch_oda_africa_site_sf %>% 
  filter(project_location_id %in% 
           (ch_oda_africa_site_intersect_adm0_sf %>% 
           filter(is.na(ISO)) %>% 
           pull(project_location_id)))
         
ch_oda_africa_site_nearest_adm0_sf <- sf::st_join(missing_adm0_sf, country_borders,
                                                    join=st_nearest_feature) 
#these all look reasonable; no need to adjust
# ch_oda_africa_site_nearest_adm0_sf %>% 
#   sf::st_drop_geometry() %>% 
#   select(project_location_id, project_title, gazetteer_adm_code, recipients_iso3, site_iso3, ISO)

#look for cases where site_iso3 doesn't match recipients_iso3
ch_oda_africa_site_intersect_adm0_sf %>% 
  sf::st_drop_geometry() %>% 
  filter(ifelse(is.na(site_iso3),-1,site_iso3) != recipients_iso3 &
           !(grepl("regional",recipient_iso3)) &
           project_location_id != "35176_373303") %>% 
  select(project_location_id, project_title, gazetteer_adm_code, recipients_iso3, site_iso3, ISO)

#many network (road, railway, pipeline) projects, so possible one country receives funding for
#projects that extend into other countries.  Write file, but keep these in the dataset as-is
#23 records
#can do a robustness check without these records also
ch_oda_africa_site_intersect_adm0_sf %>% 
  filter(ifelse(is.na(site_iso3),-1,site_iso3) != recipients_iso3 &
           !(grepl("regional",recipient_iso3)) &
           project_location_id != "35176_373303") %>% 
  st_drop_geometry() %>% 
  write.csv("./data/interim/ch_iso3_adm0_mismatch.csv", row.names=FALSE)

#add sector group columns
ch_sect_group_df <- ch_oda_africa_site_cleaned1_df %>% 
  mutate(sector_group = case_match(ad_sector_codes,
                                   #Social Infrastructure & Services
                                   110 ~ "SIS",  #Education
                                   120 ~ "SIS",  #Health
                                   130 ~ "SIS",  # Population Policies / Programmes and Reproductive Health
                                   140 ~ "SIS",  # Water Supply and Sanitation
                                   150 ~ "SIS",  #Government and Civil Society
                                   160 ~ "SIS",  #Other Social Infrastructure and Services
                                   #Economic Infrastructure & Services 
                                   210 ~ "EIS",  #Transport and Storage
                                   220 ~ "EIS",  # Communications
                                   230 ~ "EIS",  # Energy Generation and Supply
                                   240 ~ "EIS",  # Banking and Financial Services
                                   250 ~ "EIS",  # Business and Other Services
                                   # Production     
                                   310 ~ "PRO",  #Agriculture, Forestry and Fishing
                                   320 ~ "PRO",  #Industry, Mining, Construction
                                   330 ~ "PRO"  #Trade and Tourism
                                   #Leave others NA
                                  ),
        sector_group_name = case_match(sector_group,
                                 "SIS" ~ "Social Infrastructure & Services",
                                 "EIS" ~ "Economic Infrastructure & Services",
                                 "PRO" ~ "Production"))

#update sector names to be consistent between WB and China
ch_sect_group_df <- ch_sect_group_df %>% 
  mutate(ad_sector_names=case_match(ad_sector_names,
                                    "Other Social infrastructure and services" ~ "Other Social Infrastructure and Services",
                                    "General Environment Protection" ~ "General Environmental Protection",
                                    "Energy" ~ "Energy Generation and Supply",
                                    "Agriculture, Forestry, Fishing" ~ "Agriculture, Forestry and Fishing",
                                    .default = ad_sector_names)
  )


write.csv(ch_sect_group_df,"./data/interim/ch_africa_oda_sector_group.csv",row.names = FALSE)
#ch_sect_group_df <- read.csv("./data/interim/ch_africa_oda_sector_group.csv")


################################################################################
# Create a dataset with projects from both funders, exclude projects end > 2016
################################################################################
#identify columns in both datasets
intersecting_columns <- intersect(names(wb_sect_group_df), 
          names(ch_sect_group_df))

oda_sect_group_df <- bind_rows(ch_sect_group_df %>% 
                                 mutate(project_id = as.character(project_id)) %>%
                                 mutate(ad_sector_codes = as.character(ad_sector_codes)) %>%
                                 select(all_of(intersecting_columns)) %>% 
                                 mutate(funder="CH"),
                               wb_sect_group_df %>% 
                                 select(all_of(intersecting_columns)) %>% 
                                 mutate(funder="WB"),
                               ) 

# oda_sect_group_df %>% 
#   mutate(isna_txn_end_year=if_else(transactions_end_year=="",TRUE,FALSE),
#          isna_end_actual_isodate=if_else(end_actual_isodate=="",TRUE,FALSE)) %>% 
#   group_by(status,isna_txn_end_year,isna_end_actual_isodate) %>% 
#   count()
# status         isna_txn_end_year isna_end_actual_isodate     n
# <chr>          <lgl>             <lgl>                   <int>
# 1 Completion     FALSE             FALSE                   14905
# 2 Completion     FALSE             TRUE                     3776
# 3 Implementation FALSE             FALSE                   10507
# 4 Implementation FALSE             TRUE                     2482

# oda_sect_group_df %>% 
#   filter(funder=="WB") %>% 
#   group_by(status,transactions_start_year,transactions_end_year,end_actual_isodate) %>% 
#   rename(t_start=transactions_start_year,
#          t_end=transactions_end_year,
#          end_act=end_actual_isodate) %>% 
#   count() %>% 
#   print(n=50)


################################################################################
# Impute missing end years for projects, based on median length of projects by funder/sector
# transactions_end_year is not a reliable source (= transactions_start_year for CH)
#
# Both CH and WB datasets were published in 2017: 
#     If status=Completed assume 2016 end if imputed end year is later
#
# Impute end dates even for Implemenation status projects, due to data collection.
#   official completion info may not be available
################################################################################
oda_sect_group_end_df <- oda_sect_group_df %>% 
  mutate(end_year=as.integer(sub("^(\\d{4})-.*","\\1",end_actual_isodate))) %>% 
  group_by(funder, ad_sector_codes) %>% 
  mutate(all_na_end_years = all(is.na(end_year)),
         median_proj_years = ifelse(all_na_end_years,1,
                                    median(end_year - as.numeric(transactions_start_year), na.rm=TRUE)),
         imputed_end_year = as.integer(transactions_start_year + median_proj_years),
         imputed_end_year = if_else((status=="Completion" & imputed_end_year > 2016),
                                    2016,imputed_end_year),
         end_year_imputed = if_else(is.na(end_year),TRUE,FALSE),
         end_year = if_else(end_year_imputed,imputed_end_year,end_year)) %>% 
  select(-median_proj_years, -imputed_end_year, -all_na_end_years) %>% 
  ungroup()

################################################################################
# Exclude projects with end year > 2016 (no IWI estimates available after then)
################################################################################
#write them to a file first 
oda_sect_group_end_df %>% 
  filter(end_year > 2016) %>% 
  write.csv("./data/interim/oda_excluded_end_after_2016.csv")

#write file for use in further analysis
oda_sect_group_end_df %>% 
  filter(end_year <= 2016) %>% 
write.csv("./data/interim/africa_oda_sector_group_end.csv",row.names = FALSE)

#write the sector codes and names to a csv file for later use
oda_sect_group_df %>% 
  distinct(ad_sector_codes, ad_sector_names, sector_group, sector_group_name) %>%
  write.csv(.,"./data/interim/sector_group_names.csv", row.names = FALSE)

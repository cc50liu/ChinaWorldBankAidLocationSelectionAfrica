#prep_projects.R
################################################################################
# Load China and WB project data; filter to aid projects in Africa 2002-2014;
# data cleanup and preparation
################################################################################
library(dplyr)
library(sf)

rm(list=ls())

africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
################################################################################
####  World Bank                                                    
################################################################################
wb_proj <- read.csv("./data/AiddataWB1.4.2/projects.csv")
wb_locs <- read.csv("./data/AiddataWB1.4.2/locations.csv")
wb_geocoded <-  inner_join(wb_proj, wb_locs, by="project_id",multiple = "all")

wb_proj_ancillary <- read.csv("./data/AiddataWB1.4.2/projects_ancillary.csv") %>%
  filter(REGION=="AFRICA")

#narrow to projects in Africa and study context
wb_africa_oda_p3_df <- semi_join(wb_geocoded, wb_proj_ancillary, by=c("project_id"="PROJECT.ID")) %>%
  filter(transactions_start_year >= 2002 &
         precision_code %in% 1:3)  # 1=exact, 2=up to 25km, 3=adm2

#remove the objects that were used to construct wb_africa_oda_p3_df
rm(wb_proj)
rm(wb_locs)
rm(wb_geocoded)
rm(wb_proj_ancillary)

#Rows with "Unspecified" recipients_iso3 are spread across multiple locations.  Use their gazetteer_adm_code
#to construct new site_iso3 and site_iso2 columns for country of project_locations
#from data dictionary:  gazetteer_adm_code is the hierarchical order of place, 
#represented by geoname IDs, associated with the place name's geoname_id. Hierarchy is pipe delimited
#example:  6295630|6255146|KE|28|7844635
wb_africa_oda_df <- wb_africa_oda_p3_df %>%
  mutate(site_iso2 = gsub(pattern=".*\\|([A-Z]{2})\\|.*","\\1", x=gazetteer_adm_code)) %>%
  left_join(africa_isos_df, by=c("site_iso2" = "iso2")) %>%
  rename(site_iso3 = iso3) %>% 
  select(-name) %>% 
  #use recipients_iso3 for one record that has NA in site_iso3 
  mutate(site_iso3 = if_else(is.na(site_iso3),recipients_iso3, site_iso3))

rm(wb_africa_oda_p3_df)

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
                                  330 ~ "PRO",  #Trade and Tourism
                         # Direct Aid
                                  520 ~ "DIR",  #Developmental Food Aid/Food Security Assistance
                                  700 ~ "DIR",  #Emergency Response
                                  600 ~ "DIR",  #Action Relating to Debt
                                  920 ~ "DIR",  #Support to Non-governmental Organizations (NGOs) and Government Organizations
                                  530 ~ "DIR",  #Non-food commodity assistance
                         # Other
                                  410 ~ "OTH", #General Environment Protection
                                  420 ~ "OTH", #Women in Development
                                  430 ~ "OTH", #Other Multisector
                                  998 ~ "OTH"  #Unallocated / Unspecified
                                 ),
         sector_group_name = case_match(sector_group,
                                        "SIS" ~ "Social Infrastructure & Services",
                                        "EIS" ~ "Economic Infrastructure & Services",
                                        "PRO" ~ "Production",
                                        "DIR" ~ "Direct Aid",
                                        "OTH" ~ "Other"))


#update sector names to be consistent between WB and China
wb_sect_group_df <- wb_sect_group_df %>% 
  mutate(ad_sector_names=case_match(ad_sector_names,
                                    "Other Social infrastructure and services" ~ "Other Social Infrastructure and Services",
                                    "General Environment Protection" ~ "General Environmental Protection",
                                    "Energy" ~ "Energy Generation and Supply",
                                    "Agriculture, Forestry, Fishing" ~ "Agriculture, Forestry and Fishing",
                                    .default = ad_sector_names)
  )


################################################################################
# China Project Data (AidData 1.1.1)
################################################################################ 
ch_oda_df <- read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oda-like_flows.csv") 

#limit to projects with location details and study years
ch_oda_p3_df <- ch_oda_df %>%
         filter(precision_code<=3 &  
         #status %in% c("Completion","Implementation") & #dataset already limits to this
         #flow_class=="ODA-like" &  #dataset already limits to this
         umbrella==FALSE &    
         transactions_start_year <= 2014 &
         transactions_start_year >= 2002 &
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
non_africa_isos_df <- read.csv("./data/all_iso_codes.csv") %>%
  filter(region!="Africa") %>%
  select(name, alpha.3, alpha.2) %>%
  rename(iso3=alpha.3,
         iso2=alpha.2)

ch_oda_africa_p3_df <- ch_oda_p3_df %>%
  filter(!recipients_iso3 %in% non_africa_isos_df$iso3 &
         !recipients_iso3=="VNM|KHM|MMR")

#clean up non-Africa datasets
rm(ch_oda_df)
rm(ch_oda_p3_df)
rm(non_africa_isos_df)

#create site_iso2 and site_iso3 codes based on the gazetteer_adm_code
ch_oda_africa_site_df <- ch_oda_africa_p3_df %>%
  mutate(site_iso2 = gsub(pattern=".*\\|([A-Z]{2})\\|.*","\\1", x=gazetteer_adm_code)) %>%
  left_join(africa_isos_df, by=c("site_iso2" = "iso2")) %>%
  rename(site_iso3 = iso3) %>% 
  select(-name) 

#remove two records whose gazetteer locations put them in Costa Rica and Brazil
ch_oda_africa_site_cleaned_df <- ch_oda_africa_site_df %>% 
  filter(!project_location_id %in% c("31253_3399415","1020_3623076"))

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

# #can geometry help us identify correct values for differences?
# #evaluate records where spatial ISO differs from site (gazetteer based) iso
# ch_oda_africa_site_intersect_adm0_sf %>% 
#   sf::st_drop_geometry() %>% 
#   filter(ifelse(is.na(site_iso3),-1,site_iso3) != ifelse(is.na(ISO),-1,ISO) &
#          project_location_id != "35176_373303") %>% 
#   select(project_location_id, project_title, gazetteer_adm_code, recipients_iso3, site_iso3, ISO)

#in two cases, gazetteer data doesn't match the recipient_iso or spatial join
#update site_iso3 to match recipient_iso and spatial join value
ch_oda_africa_site_cleaned1_df <- ch_oda_africa_site_cleaned_df %>% 
  mutate(site_iso3 = if_else(project_location_id %in% c("30315_3346401","35629_11592387"),
                             recipients_iso3, site_iso3))
ch_oda_africa_site_intersect_adm0_sf <- ch_oda_africa_site_intersect_adm0_sf %>% 
  mutate(site_iso3 = if_else(project_location_id %in% c("30315_3346401","35629_11592387"),
                             recipients_iso3, site_iso3))

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
                                   330 ~ "PRO",  #Trade and Tourism
                                   # Direct Aid
                                   520 ~ "DIR",  #Developmental Food Aid/Food Security Assistance
                                   700 ~ "DIR",  #Emergency Response
                                   600 ~ "DIR",  #Action Relating to Debt
                                   920 ~ "DIR",  #Support to Non-governmental Organizations (NGOs) and Government Organizations
                                   530 ~ "DIR",  #Non-food commodity assistance
                                   # Other
                                   410 ~ "OTH", #General Environment Protection
                                   420 ~ "OTH", #Women in Development
                                   430 ~ "OTH", #Other Multisector
                                   998 ~ "OTH"  #Unallocated / Unspecified
                                  ),
        sector_group_name = case_match(sector_group,
                                       "SIS" ~ "Social Infrastructure & Services",
                                       "EIS" ~ "Economic Infrastructure & Services",
                                       "PRO" ~ "Production",
                                       "DIR" ~ "Direct Aid",
                                       "OTH" ~ "Other"))

#update sector names to be consistent between WB and China
ch_sect_group_df <- ch_sect_group_df %>% 
  mutate(ad_sector_names=case_match(ad_sector_names,
                                    "Other Social infrastructure and services" ~ "Other Social Infrastructure and Services",
                                    "General Environment Protection" ~ "General Environmental Protection",
                                    "Energy" ~ "Energy Generation and Supply",
                                    "Agriculture, Forestry, Fishing" ~ "Agriculture, Forestry and Fishing",
                                    .default = ad_sector_names)
  )

################################################################################
# Create a dataset with projects from both funders
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

#write file for use in further analysis
oda_sect_group_df %>% 
  write.csv("./data/interim/africa_oda_sector_group.csv",row.names = FALSE)

#write the sector codes and names to a csv file for later use
oda_sect_group_df %>% 
  distinct(ad_sector_codes, ad_sector_names, sector_group, sector_group_name) %>%
  write.csv(.,"./data/interim/sector_group_names.csv", row.names = FALSE)

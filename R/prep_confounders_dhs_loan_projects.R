#prep_confounders_dhs_loan_projects.R
library(dplyr)
library(ggplot2)
rm(list=ls())

#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

####################################################
#### Load administrative borders
projection <- "ESRI:102023"  #WGS 1984 Equidistant Conic for Africa.

adm1_sf <- sf::st_read("./data/country_regions/gadm1_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm1_sf) = "EPSG:4326"
adm1_sf <- sf::st_transform(adm1_sf,crs=sf::st_crs(projection))
adm1_sf <- sf::st_make_valid(adm1_sf)
unique(sf::st_is_valid(adm1_sf))

adm2_sf <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_sf) = "EPSG:4326"
adm2_sf <- sf::st_transform(adm2_sf,crs=sf::st_crs(projection))
adm2_sf <- sf::st_make_valid(adm2_sf)
unique(sf::st_is_valid(adm2_sf))

####################################################
#### Loan projects
#read projects and limit to those with location details and study years
#n=346 #safe to limit to africa isos because rows with multiple recipients not in Africa
#these all have a status of Completion or Implementation
ch_oof_df <- read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oof-like_flows.csv") %>%  
  filter(precision_code<=4 &  # 1=exact, 2=up to 25km, 3=ADM2, or 4=ADM1 
           umbrella==FALSE &
           year <= 2014 &
           !is.na(latitude) &
           recipients_iso3 %in% africa_isos_df$iso3)
#start_actual_isodate has too many missing values, so use transaction start date instead

#convert to spatial sf object
ch_oof_sf <- ch_oof_df %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
  select(project_location_id,precision_code,transactions_start_year,geometry) %>%
  sf::st_transform(.,crs=sf::st_crs(projection))

#give the projects a footprint equal to the ADM1 or ADM2 hosting them
ch_adm_oof_sf <- bind_rows(
  #ADM1 projects have adm1 footprint
  sf::st_join(adm1_sf, 
              ch_oof_sf[ch_oof_sf$precision_code==4,], 
              join = sf::st_intersects, left = FALSE) %>%
  select(ID_adm1,transactions_start_year,project_location_id,precision_code,geometry) %>% 
  rename(ID_adm=ID_adm1),
  #exact, near, and ADM2 projects have adm2 footprint
  sf::st_join(adm2_sf, 
            ch_oof_sf[ch_oof_sf$precision_code %in% 1:3, ], 
            join = sf::st_intersects, left = FALSE) %>%
  select(ID_adm2,transactions_start_year,project_location_id,precision_code,geometry) %>% 
  rename(ID_adm=ID_adm2)
)

#three project locations didn't get matched--all with precision 1:3
missing_sf <- ch_oof_sf %>%
  anti_join(ch_adm_oof_sf %>%
              sf::st_drop_geometry())

#make a version of the adm2_sf object with no geometry for field updates
adm2_df <- adm2_sf %>% sf::st_drop_geometry()

#for missing projects, use the nngeo package to find nearest match
missing_nn_sf <- missing_sf %>% 
    mutate(missing_index = unlist(nngeo::st_nn(.,adm2_sf)),
           ID_adm = adm2_df[missing_index,"ID_adm2"],
           adm2_geometry = sf::st_geometry(adm2_sf[missing_index, ]),
           geometry = adm2_geometry)

ch_adm_oof_sf <- rbind(ch_adm_oof_sf,
                       missing_nn_sf %>% 
                         select(ID_adm,transactions_start_year,project_location_id,
                                precision_code, geometry))

####################################################
#### Read DHS survey points
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")

dhs_sf <- dhs_df  %>%
  sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,geometry) %>% 
  sf::st_transform(dhs_sf,crs=sf::st_crs(projection))

#intersect projects with dhs points n=2084
dhs_proj_intersect_df <- sf::st_join(dhs_sf, ch_adm_oof_sf, 
                                     join = sf::st_intersects, left=FALSE)  %>%
  sf::st_drop_geometry() %>%
  group_by(dhs_id, transactions_start_year)  %>%
  summarize(proj_count = n()) %>%
  ungroup() 

#create rows with 0's for years with no projects n=24912
dhs_proj_inter_fill_df <- dhs_proj_intersect_df %>% 
  tidyr::complete(dhs_id, transactions_start_year = 1999:2014,
                  fill = list(proj_count = 0)) 

#convert to a wide format n=1557
dhs_proj_wide_df <- dhs_proj_inter_fill_df %>% 
  tidyr::pivot_wider(names_from=transactions_start_year,
                     values_from = proj_count,
                     names_prefix = "ch_loan_proj_n_") 


# Store the proj counts in the dhs_df and create logged versions of them
dhs_final_df <- dhs_df %>%
  left_join(dhs_proj_wide_df, by = "dhs_id") %>%
  mutate(across(starts_with("ch_loan_proj_n_"), ~ replace(., is.na(.), 0))) %>% 
  mutate(across(starts_with("ch_loan_proj_n_"), ~ log(. + .01), .names = "log_{.col}"))  

write.csv(dhs_final_df,"./data/interim/dhs_loan_projs.csv",row.names=FALSE)
#dhs_final_df <- read.csv("./data/interim/dhs_loan_projs.csv")

#plot the distribution of loan-based projects        
loan_projs <- dhs_final_df %>%
  tidyr::pivot_longer(cols = starts_with("ch_loan_proj_n_"), names_to = "transactions_start_year", values_to = "projects_n") %>%
  ggplot(aes(projects_n, color = transactions_start_year)) +
  geom_density() +
  labs(x = "Count of Chinese loan-based projects", y = "Density across DHS clusters",
       title = "Chinese loan-based project counts across DHS", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()

loan_projs
ggsave("./figures/ch_loan_projs.png",loan_projs, width=6, height = 6, dpi=300,
       bg="white", units="in")

log_loan_projs <-  dhs_final_df %>%
  tidyr::pivot_longer(cols = starts_with("log_ch_loan_proj_n_"), names_to = "transactions_start_year", values_to = "log_projects_n") %>%
  ggplot(aes(log_projects_n, color = transactions_start_year)) +
  geom_density(aes(y=after_stat(density))) +
  labs(x = "Count (log) of Chinese loan-based projects", y = "Density across DHS clusters",
       title = "Chinese loan-based project counts (log) across DHS clusters", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()

log_loan_projs
ggsave("./figures/log_ch_loan_projs.png",log_loan_projs, width=6, height = 5, dpi=300,
       bg="white", units="in")

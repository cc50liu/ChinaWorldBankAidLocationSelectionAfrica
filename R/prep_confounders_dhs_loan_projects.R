#prep_confounders_dhs_loan_projects.R
library(dplyr)
library(ggplot2)
rm(list=ls())

#get Africa ISO codes
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")

####################################################
#### Load administrative borders
projection <- "ESRI:102023"

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
#### Transport projects
#read projects and limit to those with location details and study years
#n=267 #safe to limit to africa isos because rows with multiple recipients not in Africa
#these all have a status of Completion or Implementation
ch_transport_oof_df <- read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oof-like_flows.csv") %>%  
  filter(precision_code<=4 &  # 1=exact, 2=up to 25km, 3=ADM2, or 4=ADM1 
           umbrella==FALSE &
           year <= 2013 &
           !is.na(latitude) &
           ad_sector_names=="Transport and Storage" &
           recipients_iso3 %in% africa_isos_df$iso3)
#start_actual_isodate has too many missing values, so use transaction start date instead

#projects from 2005:2013
# ch_transport_oof_df %>% 
#   group_by(transactions_start_year) %>% 
#   count()

ch_transport_oof_sf <- ch_transport_oof_df %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
  select(project_location_id,precision_code,transactions_start_year,geometry) %>%
  sf::st_transform(.,crs=sf::st_crs(projection))

#give the projects a footprint equal to the ADM1 or ADM2 hosting them
ch_adm_transport_oof_sf <- bind_rows(
  #ADM1 projects have adm1 footprint
  sf::st_join(adm1_sf, 
              ch_transport_oof_sf[ch_transport_oof_sf$precision_code==4,], 
              join = sf::st_intersects, left = FALSE) %>%
  select(ID_adm1,transactions_start_year,project_location_id,precision_code,geometry) %>% 
  rename(ID_adm=ID_adm1),
  #exact, near, and ADM2 projects have adm2 footprint
  sf::st_join(adm2_sf, 
            ch_transport_oof_sf[ch_transport_oof_sf$precision_code %in% 1:3, ], 
            join = sf::st_intersects, left = FALSE) %>%
  select(ID_adm2,transactions_start_year,project_location_id,precision_code,geometry) %>% 
  rename(ID_adm=ID_adm2)
)

# ch_adm_transport_oof_sf %>% 
#   sf::st_drop_geometry() %>% 
#   group_by(ID_adm, transactions_start_year )

####################################################
#### Read DHS survey points
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")

dhs_sf <- dhs_df  %>%
  sf::st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,geometry) %>% 
  sf::st_transform(dhs_sf,crs=sf::st_crs(projection))

#intersect projects with dhs points n=160
dhs_proj_intersect_df <- sf::st_join(dhs_sf, ch_adm_transport_oof_sf, 
                                     join = sf::st_intersects, left=FALSE)  %>%
  sf::st_drop_geometry() %>%
  group_by(dhs_id, transactions_start_year)  %>%
  summarize(proj_count = n()) %>%
  ungroup() 

#create rows with 0's for years with no projects n=2400
dhs_proj_inter_fill_df <- dhs_proj_intersect_df %>% 
  tidyr::complete(dhs_id, transactions_start_year = 1999:2013,
                  fill = list(proj_count = 0)) 

#create a cumulative_count column
dhs_proj_cum_df <- dhs_proj_inter_fill_df %>% 
  arrange(dhs_id, transactions_start_year) %>% 
  group_by(dhs_id) %>% 
  mutate(cumulative_count = cumsum(proj_count)) %>% 
  ungroup()

#convert to a wide format n=160
dhs_proj_cum_wide_df <- dhs_proj_cum_df %>% 
  select(-proj_count) %>% 
  tidyr::pivot_wider(names_from=transactions_start_year,
                     values_from = cumulative_count) %>% 
  rename_with(~ gsub("^([0-9]{4}$)", "trans_proj_cum_n_\\1", .),
              where(is.numeric))
                     
# Store the proj counts in the dhs_df and create logged versions of them
dhs_final_df <- dhs_df %>%
  left_join(dhs_proj_cum_wide_df, by = "dhs_id") %>%
  mutate(across(starts_with("trans_proj_cum_n_"), ~ replace(., is.na(.), 0))) %>% 
  mutate(across(starts_with("trans_"), ~ log(. + 1), .names = "log_{.col}"))  

write.csv(dhs_final_df,"./data/interim/dhs_loan_projs.csv",row.names=FALSE)
#dhs_final_df <- read.csv("./data/interim/dhs_loan_projs.csv")

#plot the distribution of loan-based transport projects        
transport_projs <- dhs_final_df %>%
  tidyr::pivot_longer(cols = starts_with("trans_proj_cum_n_"), names_to = "transactions_start_year", values_to = "cum_projects_n") %>%
  ggplot(aes(cum_projects_n, color = transactions_start_year)) +
  geom_density() +
  labs(x = "Cumulative count of loan-based transport projects", y = "Density across DHS clusters",
       title = "Cumulative loan-based transport project counts across DHS", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()

transport_projs
ggsave("./figures/transport_projs.png",transport_projs, width=6, height = 4, dpi=300,
       bg="white", units="in")

log_transport_projs <-  dhs_final_df %>%
  tidyr::pivot_longer(cols = starts_with("log_trans_proj_cum_n_"), names_to = "transactions_start_year", values_to = "log_cum_projects_n") %>%
  ggplot(aes(log_cum_projects_n, color = transactions_start_year)) +
  geom_density(aes(y=after_stat(density))) +
  labs(x = "Cumulative count (log) of loan-based transport projects", y = "Density across DHS clusters",
       title = "Loan-based transport project counts (log) across DHS", color="Year") +
  scale_color_discrete(labels = function(x) gsub(".*?(\\d{4})$", "\\1", x)) +
  theme_bw()

log_transport_projs
ggsave("./figures/log_transport_projs.png",log_transport_projs, width=6, height = 4, dpi=300,
       bg="white", units="in")

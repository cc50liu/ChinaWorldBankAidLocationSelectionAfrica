# select_dhs_treat_control_5k.R
# Compare DHS cluster locations with project locations to select treated and 
# and control communities for annual, 3yr image group runs by sector and by
# sector group.
library(dplyr)
library(sf)
library(stringr)
library(tidyr)

rm(list=ls())

################################################################################
# Function preparing annual project footprints 
################################################################################
prep_projects <- function(oda_df, adm2_sf, projection, sector, start_year, debug_msg)
{
  #uncomment for testing 
  # oda_df <- ch_oda_df
  # sector <- 110
  # adm2_sf <- adm2_borders

  sect_oda_df  <- oda_df %>%
    filter(ad_sector_codes==sector &
           transactions_start_year==start_year) 

  if (nrow(sect_oda_df) > 0) {
    oda_sf <- sect_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,transactions_start_year,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector ", 
                                sector,", year ",start_year,": ",nrow(oda_sf)))
    
    #consider the project footprint to be the ADM2 in which it is hosted
    oda_buff_sf <- st_join(adm2_sf, oda_sf, join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, transactions_start_year, geometry)
    
    #some locations are outside official ADM2 borders (like in a bay), find nearest
    if (nrow(oda_sf) != nrow(oda_buff_sf)) {
      un_original <- length(unique(oda_sf$geoname_id))
      un_buffer <- length(unique(oda_buff_sf$geoname_id))
      print(paste("Buffer has", nrow(oda_sf) - nrow(oda_buff_sf),
                  "fewer than original. Unique geonames in original:", un_original,
                  "buffer:",un_buffer))
      if (un_buffer < un_original) 
      {
        
        not_in_oda_buff <- !(oda_sf$geoname_id %in% oda_buff_sf$geoname_id)
        missing_sf <- oda_sf[not_in_oda_buff, ]
        
        missing_nn_sf <- missing_sf %>% 
          mutate(adm2_id = unlist(nngeo::st_nn(.,adm2_sf)),
                 adm2_geometry = st_geometry(adm2_sf[adm2_id, ]),
                 geometry = adm2_geometry) 
        print(paste("nngeo found", nrow(missing_nn_sf[st_geometry_type(missing_nn_sf)!="POINT",])))
        
        oda_buff_sf <- rbind(oda_buff_sf,
                             missing_nn_sf %>% 
                               select(geoname_id,precision_code,
                                      transactions_start_year, geometry))

      }
    }

  } else {
    #create empty sf object
    oda_buff_sf <- sf::st_sf(geometry = sf::st_sfc())
    print(paste0("No project locations for sector ",sector,", year ",start_year))
  } 
  
  return(oda_buff_sf)
}


################################################################################
# Function that spatially intersects projects and dhs points per sector, for
# study years
################################################################################
process_sectors <- function(sector, projection, dhs_buff_sf,wb_oda_df, ch_oda_df,
                            adm2_borders, debug_msg) 
{
  #uncomment to test
  #sector <- 220
  #dhs_buff_sf <- dhs_sf
  #start_year = 2002
  
  start_years <- 2001:2014

  sector_output <- lapply(start_years, function(start_year) {
    #uncomment to test
    #start_year = 2002
    
    ####################################################
    #### Prepare project location data for the year
    if (exists("wb_oda_df", inherits=TRUE)) {
      wb_oda_sect_buff_sf <- prep_projects(wb_oda_df,adm2_borders, projection, 
                                           sector, start_year, debug_msg)
  
      if (debug_msg & (nrow(wb_oda_sect_buff_sf) > 0))
        print(paste0("Locations in wb_oda_sect_buff_sf for sector ", 
                              sector,", year ",start_year,": ",nrow(wb_oda_sect_buff_sf)))
    } else if (debug_msg) {
      print(paste("No WB locations for sector",sector))
    }
    
    if (exists("ch_oda_df", inherits=TRUE)) {
      ch_oda_sect_buff_sf <- prep_projects(ch_oda_df,adm2_borders, projection, 
                                           sector, start_year, debug_msg)
      
      if (debug_msg & (nrow(ch_oda_sect_buff_sf) > 0))
        print(paste0("Locations in ch_oda_sect_buff_sf for sector ", 
                              sector,", year ",start_year,": ",nrow(ch_oda_sect_buff_sf)))
    } else if (debug_msg) {
      print(paste("No CH locations for sector and year ",sector,start_year))
    }
    
    ############################################################
    # Identify treatment and control DHS points
    ############################################################
    #Treated points intersect with at least one project
    #initialize empty dataframes for later rbind
    return_wb_df <- return_ch_df <- data.frame(funder=character(),
                               start_year=integer(),
                               dhs_id = integer(),
                               proj_count = integer())
    
    #World Bank
    if (nrow(wb_oda_sect_buff_sf) > 0) {
      wb_df <- st_join(dhs_buff_sf,
                          wb_oda_sect_buff_sf,
                          left=FALSE) %>%
        st_drop_geometry() 

      #did at least one dhs point intersect with the project?
      #nrow can be 0 if project is in a non-DHS country
      if (nrow(wb_df) > 0) {
        wb_df <- wb_df %>% 
          group_by(dhs_id) %>%
          summarize(proj_count = n()) %>%  
          ungroup()
        
        #prep return dataframe
        return_wb_df <- cbind(funder="wb",start_year,wb_df)
      } else if (debug_msg) {
        print(paste("No wB projects intersect with DHS points for sector/year ",sector,start_year))
      }     
    }  
            
    #China  
    if (nrow(ch_oda_sect_buff_sf) > 0) {  
      ch_df <- st_join(dhs_buff_sf,
                          ch_oda_sect_buff_sf,
                          left=FALSE) %>%
        st_drop_geometry() 
      
      #did at least one dhs point intersect with the project?
      #can happen if project is in a non-DHS country
      if (nrow(ch_df) > 0) {
        ch_df <- ch_df %>% 
          group_by(dhs_id) %>%
          summarize(proj_count = n()) %>%  
          ungroup()
      
        #prep return dataframe
        return_ch_df <- cbind(funder="ch",start_year,ch_df)
      } else if (debug_msg) {
        print(paste("No CH projects intersect with DHS points for sector/year ",sector,start_year))
      } 
    }
    
    return(rbind(return_wb_df,return_ch_df))
  })
  
  all_years_funders <- do.call(rbind,sector_output)
  return(cbind(sector,all_years_funders))
}



################################################################################
# main code starts here
################################################################################
wb_oda_df <- read.csv("./data/interim/africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
         funder=="WB" & 
         transactions_start_year>=2002) 
ch_oda_df <- read.csv("./data/interim/africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
        funder=="CH" & 
          transactions_start_year>=2002) 

africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
debug_msg=as.logical(TRUE)

#generate vectors of countries and sectors to pass to function
dhs_isos_v <- read.csv("./data/AIGlobalLab/dhs_clusters.csv") %>%
  filter(year %in% 2002:2019) %>% 
  mutate(iso3 = substr(GID_1, 1, 3))
  select(iso3) %>%
  distinct()  %>%
  pull(iso3)

sectors_v <- unique(rbind(
  wb_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(ad_sector_codes),
  ch_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(ad_sector_codes))) %>%
  arrange(ad_sector_codes) %>%
  pull(ad_sector_codes)

projection <- "ESRI:102023"   #WGS 1984 Equidistant Conic for Africa.

####################################################
#### Load administrative borders
####################################################
adm2_borders <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- sf::st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
unique(sf::st_is_valid(adm2_borders))

####################################################
#### Process DHS survey points
####################################################
dhs_df <- read.csv("./data/AIGlobalLab/dhs_clusters.csv") %>%
  filter(year %in% 2005:2019) %>% 
  mutate(iso3 = substr(GID_1, 1, 3))

dhs_sf <- dhs_df  %>%
  st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,year,iso3,rural,geometry) 

dhs_sf <- sf::st_transform(dhs_sf,crs=st_crs(projection))
dhs_sf <- sf::st_make_valid(dhs_sf)
unique(sf::st_is_valid(dhs_sf))

if (debug_msg) print(paste("Obs in dhs_sf:",nrow(dhs_sf)))

####################################################
# call main function to process sectors
####################################################
output <- lapply(sectors_v, function(sector) {
  process_sectors(sector, projection, dhs_sf, wb_oda_df,
                  ch_oda_df, adm2_borders, debug_msg)
})

all_sectors <- do.call(rbind,output)
#n=86295
#write to intermediate file for use during development
write.csv(all_sectors,"./data/interim/all_sectors_actual.csv",row.names=FALSE)

#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector","funder","start_year","dhs_id","proj_count","year_group","sector_group")

#get sector group names
sector_groups_df <- read.csv("./data/interim/sector_group_names.csv")
  
#add year group columns n=86295
all_sectors_expanded_df <- all_sectors %>% 
  left_join(sector_groups_df, join_by(sector==ad_sector_codes)) %>% 
  mutate(year_group = case_when(
            start_year %in% 1999:2001 ~ '1999:2001',
            start_year %in% 2002:2004 ~ '2002:2004',
            start_year %in% 2005:2007 ~ '2005:2007',
            start_year %in% 2008:2010 ~ '2008:2010',
            start_year %in% 2011:2013 ~ '2011:2013',
            start_year %in% 2014:2016 ~ '2014:2016'
        ))  %>% 
  select(all_of(column_order))

#write for annual run by sector
write.csv(all_sectors_expanded_df,"./data/interim/dhs_treated_sector_actual_annual.csv",row.names=FALSE)
#all_sectors_expanded_df <- read.csv("./data/interim/dhs_treated_sector_annual.csv")

########################################################
# calculate annual spillover variables
########################################################
#count annual treatments by adm2
annual_adm2_treated <- all_sectors_expanded_df %>%
  #get ID_adm2 from dhs_df
  left_join(dhs_df, by="dhs_id") %>% 
  #dhs points in the ID_adm2 share same proj_count for time period, get it once
  group_by(ID_adm2,sector,funder,start_year,proj_count) %>% 
  summarize(count_observations = n()) %>% 
  #get total proj count for all sectors and funders for each adm2  
  group_by(ID_adm2, start_year) %>% 
  summarize(total_projs = sum(proj_count)) %>% 
  ungroup()

#calculate annual treatment counts in neighboring adm2s
adjacent_adm2_count_df <- read.csv("./data/interim/adjacent_adm2_actual.csv") %>% 
  left_join(annual_adm2_treated, by = c("neighbor" = "ID_adm2"),
                       multiple="all") %>% 
  filter(!is.na(start_year)) %>% 
  group_by(ID_adm2, start_year) %>%
  summarize(total_neighbor_projs = sum(total_projs, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(log_total_neighbor_projs = log(1 + total_neighbor_projs))


#write neighbor annual treatment counts 
write.csv(adjacent_adm2_count_df,"./data/interim/adm2_adjacent_annual_treat_count_actual.csv",row.names=FALSE)

#plot the distribution of neighbor annual treatment counts
library(ggplot2)

neighbor_projs <- adjacent_adm2_count_df %>% 
  mutate(year_color=as.factor(start_year)) %>% 
  ggplot(aes(total_neighbor_projs, color=year_color)) +
  geom_density() +
  labs(x = "Project counts (all sectors & funders) in ADM2 neighbors", 
       y = "Density across DHS points",
       title="Project counts (all sectors & funders) in ADM2 neighbors",
       color="Year")  +
  theme_bw()

ggsave("./figures/neighbor_adm2_proj_n_actual.png",neighbor_projs,
       width=6, height = 6, dpi=300,
       bg="white", units="in")

log_neighbor_projs <- adjacent_adm2_count_df %>% 
  mutate(year_color=as.factor(start_year)) %>% 
  ggplot(aes(log_total_neighbor_projs, color=year_color)) +
  geom_density() +
  labs(x = "Project counts (all sectors & funders, log + 1) in ADM2 neighbors", 
       y = "Density across DHS points",
       title="Project counts in ADM2 neighbors (log + 1)",
       color="Year")  +
  theme_bw()

ggsave("./figures/neighbor_adm2_proj_logged_n_actual.png",log_neighbor_projs,
       width=6, height = 6, dpi=300,
       bg="white", units="in")

#####################################
# year group records
#####################################
treated_year_group <- all_sectors_expanded_df %>% 
  group_by(sector, funder, dhs_id, year_group) %>% 
  summarize(sum_proj_count = sum(proj_count, na.rm=TRUE)) %>% 
  rename(proj_count = sum_proj_count) %>% 
  ungroup()
#n=76256

write.csv(treated_year_group,"./data/interim/dhs_treated_sector_3yr_actual.csv",row.names=FALSE)

########################################################
# calculate 3yr spillover variables
########################################################
#count 3yr treatments by adm2
adm2_treated_3yr <- treated_year_group %>%
  #get ID_adm2 from dhs_df
  left_join(dhs_df, by="dhs_id") %>% 
  #dhs points in the ID_adm2 share same proj_count for time period, get it once
  group_by(ID_adm2,sector,funder,year_group,proj_count) %>% 
  summarize(count_observations = n()) %>% 
  #get total proj count for all sectors and funders for each adm2  
  group_by(ID_adm2, year_group) %>% 
  summarize(total_projs = sum(proj_count)) %>% 
  ungroup()

#calculate 3yr treatment counts in neighboring adm2s
adjacent_adm2_3yr_count_df <- read.csv("./data/interim/adjacent_adm2_actual.csv") %>% 
  left_join(adm2_treated_3yr, by = c("neighbor" = "ID_adm2"),
            multiple="all") %>% 
  filter(!is.na(year_group)) %>% 
  group_by(ID_adm2, year_group) %>%
  summarize(total_neighbor_projs = sum(total_projs, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(log_total_neighbor_projs = log(1 + total_neighbor_projs))


#write neighbor annual treatment counts 
write.csv(adjacent_adm2_3yr_count_df,"./data/interim/adm2_adjacent_3yr_treat_count.csv",row.names=FALSE)

#look at counts
treated_year_group %>%
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  ungroup() %>% 
  print (n=100)

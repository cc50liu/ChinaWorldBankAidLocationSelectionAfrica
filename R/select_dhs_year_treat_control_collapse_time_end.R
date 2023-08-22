# select_dhs_year_treat_control_collapse_time_end.R
# Compare DHS cluster locations with project locations to select treatment and controls,
# min start year and max end years. No imputed end years in input data
library(dplyr)
library(sf)
library(stringr)

rm(list=ls())

################################################################################
# Function preparing project footprints 
################################################################################
prep_projects <- function(oda_df, adm2_sf, projection, sector, debug_msg)
{
  #uncomment for testing 
   # oda_df <- ch_oda_df
   # sector <- 110
   # adm2_sf <- adm2_borders
  
  sect_oda_df  <- oda_df %>%
    filter(ad_sector_codes==sector) 

  if (nrow(sect_oda_df) > 0) {
    oda_sf <- sect_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,transactions_start_year,end_year,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector ", 
                                sector,": ",nrow(oda_sf)))
    
    #consider the project footprint to be the ADM2 in which it is hosted
    oda_buff_sf <- st_join(adm2_sf, oda_sf, join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, transactions_start_year, end_year, geometry)
    
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
                               select(geoname_id,precision_code,transactions_start_year,end_year,geometry))

      }
    }

  } else {
    #create empty sf object
    oda_buff_sf <- sf::st_sf(geometry = sf::st_sfc())
    print(paste("No project locations for sector",sector))
  } 
  
  return(oda_buff_sf)
}


################################################################################
# Function that spatially intersects projects and dhs points per sector
################################################################################
process_sectors <- function(sector, projection, dhs_buff_sf,wb_oda_df, ch_oda_df,
                            adm2_borders, debug_msg) 
{
  #uncomment to test
  #sector <- 110
  #dhs_buff_sf <- dhs_sf

  ####################################################
  #### Prepare project location data 
  if (exists("wb_oda_df", inherits=TRUE)) {
    wb_oda_sect_buff_sf <- prep_projects(wb_oda_df,adm2_borders, projection, 
                                         sector, debug_msg)

    if (debug_msg & (nrow(wb_oda_sect_buff_sf) > 0)) {
      print(paste0("Locations in wb_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(wb_oda_sect_buff_sf))) 
    }
  } else if (debug_msg) {
    print(paste("No WB locations for sector",sector))
  }
  
  if (exists("ch_oda_df", inherits=TRUE)) {
    ch_oda_sect_buff_sf <- prep_projects(ch_oda_df,adm2_borders, projection, 
                                         sector, debug_msg)
    
    if (debug_msg & (nrow(ch_oda_sect_buff_sf) > 0)) {
      print(paste0("Locations in ch_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(ch_oda_sect_buff_sf)))
    }
  } else if (debug_msg) {
    print(paste("No CH locations for sector",sector))
  }
  
  ############################################################
  # Identify treatment and control DHS points
  ############################################################
  #Treated points intersect with at least one project
  #initialize empty dataframes for later rbind
  return_wb_df <- return_ch_df <- data.frame(funder=character(),
                                             min_start_year=integer(),
                                             dhs_id = integer(),
                                             max_end_year = integer(),
                                             proj_count = integer())  
  
  
  #World Bank
  if (nrow(wb_oda_sect_buff_sf) > 0) {
    wb_df <- st_join(dhs_buff_sf,
                     wb_oda_sect_buff_sf,
                     left=FALSE) %>%
      st_drop_geometry() 
    
    #did at least one dhs point intersect with the project?
    #can happen that none do if project is in a non-DHS country
    if (nrow(wb_df) > 0) {
      wb_df <- wb_df %>% 
        group_by(dhs_id) %>%
        summarize(min_start_year= min(transactions_start_year, na.rm = TRUE),
                  #if end_year is.na, assume it is 2016 before calculating max
                  max_end_year = as.integer(max(
                    ifelse(is.na(end_year),2016,end_year))),
                  proj_count = n()) %>%  
        ungroup()

      #prep return dataframe
      return_wb_df <- cbind(funder="wb",wb_df)
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
    #can happen that none do if project is in a non-DHS country
    if (nrow(ch_df) > 0) {
      ch_df <- ch_df %>% 
        group_by(dhs_id) %>%
        summarize(min_start_year= min(transactions_start_year, na.rm = TRUE),
                  #if end_year is.na, assume it is 2016 before calculating max
                  max_end_year = as.integer(max(
                    ifelse(is.na(end_year),2016,end_year))),
                  proj_count = n()) %>%  
        ungroup()
      
      #prep return dataframe
      return_ch_df <- cbind(funder="ch",ch_df)
    } else if (debug_msg) {
      print(paste("No ch projects intersect with DHS points for sector/year ",sector,start_year))
    }     
  }   
  
  both_funders <- rbind(return_wb_df,return_ch_df)
  return(cbind(sector,both_funders))
         
}

################################################################################
# main code starts here
################################################################################
wb_oda_df <- read.csv("./data/interim/africa_oda_sector_group_end_noimpute.csv") %>% 
  filter(precision_code %in% c(1,2,3)  & #Exact, near, ADM2
         funder=="WB") 
ch_oda_df <- read.csv("./data/interim/africa_oda_sector_group_end_noimpute.csv") %>% 
  filter(precision_code %in% c(1,2,3)  & #Exact, near, ADM2
           funder=="CH") 

africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
debug_msg=as.logical(TRUE)

#generate vectors of countries and sectors to pass to function
dhs_isos_v <- read.csv("./data/interim/dhs_est_iwi.csv") %>%
  select(iso3) %>%
  distinct()  %>%
  pull(iso3)

sectors_v <- unique(rbind(
  wb_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(ad_sector_codes),
  ch_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(ad_sector_codes))) %>%
  arrange(ad_sector_codes) %>%
  pull(ad_sector_codes)

projection <- "ESRI:102023"

#####################################
#### Load administrative borders
#####################################
adm2_borders <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- sf::st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
unique(sf::st_is_valid(adm2_borders))

#####################################
#### Process DHS survey points
#####################################
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")

dhs_sf <- dhs_df  %>%
  st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,year,iso3,rural,geometry) 

dhs_sf <- sf::st_transform(dhs_sf,crs=st_crs(projection))
dhs_sf <- sf::st_make_valid(dhs_sf)
unique(sf::st_is_valid(dhs_sf))

if (debug_msg) print(paste("Obs in dhs_sf:",nrow(dhs_sf)))


####################################################
output <- lapply(sectors_v, function(sector) {
  process_sectors(sector, projection, dhs_sf, wb_oda_df,
                  ch_oda_df, adm2_borders, debug_msg)
})

all_sectors <- do.call(rbind,output)

#############################
# handle treated locations
#############################
#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector","funder","dhs_id","min_start_year","max_end_year","proj_count")

#make adjustments when locations were treated by both funders
treated_both <- all_sectors %>%
  group_by(dhs_id, sector) %>%
  filter(any(funder == "wb") && any(funder == "ch")) %>%
  ungroup()

#remove the individual funder records when treated by both
all_sectors_less_both <- all_sectors %>% 
  anti_join(treated_both, by=c("dhs_id", "sector"))

#prepare "both" rows to add back   
both_to_add <- treated_both %>% 
  group_by(dhs_id, sector) %>% 
  summarize(both_max_end_year=max(max_end_year),
            both_proj_count=sum(proj_count),
            both_min_start_year=min(min_start_year)) %>% 
  mutate(both_funder="both") %>% 
  ungroup() %>% 
  rename(max_end_year=both_max_end_year,
         proj_count=both_proj_count,
         funder=both_funder,
         min_start_year = both_min_start_year) %>% 
  select(!!!lapply(column_order,sym))

#add "both" rows and an additional image_group column
all_sectors_w_both <- rbind(all_sectors_less_both,both_to_add) 

# all_sectors_w_both %>% 
#   group_by(sector,funder) %>% 
#   count() %>% 
#   print(n=43)

#############################
# handle control locations
#############################
#generate all possible combinations of sector and dhs_ids
expanded_dhs <- expand.grid(sector = sectors_v,
                            dhs_id = dhs_df$dhs_id) 

######## identify control points, excluding those treated by either funder
control_points <- expanded_dhs %>% 
  anti_join(all_sectors_w_both, by=c("dhs_id", "sector")) %>% 
  mutate(funder="control",
         min_start_year=NA_integer_,
         max_end_year=NA_integer_,
         proj_count=0) %>% 
  select(!!!lapply(column_order,sym))
#n=163,323

#n=208110
dhs_treat_control_long <- rbind(all_sectors_w_both,control_points)

# dhs_treat_control_long %>%
#   group_by(sector,funder) %>%
#   count() %>% 
#   print(n=64)

write.csv(dhs_treat_control_long,"./data/interim/dhs_treat_control_collaps_end_dates.csv",row.names=FALSE)




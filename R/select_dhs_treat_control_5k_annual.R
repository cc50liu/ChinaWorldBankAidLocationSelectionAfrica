# select_dhs_treat_control_5k_annual.R
# Compare DHS cluster locations with project locations to select annual treatment 
# and controls.
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
      #can happen if project is in a non-DHS country
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
wb_oda_df <- read.csv("./data/interim/africa_oda_sector_group_v2.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
         funder=="WB" & 
         transactions_start_year>=2002) 
ch_oda_df <- read.csv("./data/interim/africa_oda_sector_group_v2.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
        funder=="CH" & 
          transactions_start_year>=2002) 

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
dhs_df <- read.csv("./data/interim/dhs_est_iwi_annual.csv")

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
#n=86295

#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector","funder","start_year","dhs_id","proj_count")

#identify places that had aid from both funders in a year - n=1933
treated_both <- all_sectors %>%
  group_by(dhs_id, start_year, sector) %>% 
  filter(any(funder == "wb") && any(funder == "ch")) %>% 
  distinct(dhs_id, start_year, sector) %>% 
  ungroup() %>% 
  mutate(treated_both_funders = 1)  
  
#add "treated_both_funders" column 
all_sectors_both_column <- all_sectors %>% 
  left_join(treated_both, by=c("dhs_id", "start_year", "sector")) %>% 
  mutate(treated_both_funders = ifelse(is.na(treated_both_funders),0,treated_both_funders))

#generate all possible combinations of sector, start_year, and dhs_ids, n=3,052,280
expanded_dhs <- expand.grid(sector = sectors_v,
                           start_year = as.integer(2001:2014),
                           dhs_id = dhs_df$dhs_id) %>% 
  mutate(treated_both_funders = 0)

######## identify control points, excluding those treated each year, n=2,967,918
control_points <- expanded_dhs %>% 
  anti_join(all_sectors_both_column, by=c("dhs_id", "start_year", "sector")) %>% 
  mutate(funder="control",
         proj_count=0) %>% 
  select(!!!lapply(column_order,sym), treated_both_funders)

#n=3,054,213
dhs_treat_control_long <- rbind(all_sectors_both_column,control_points)

# dhs_treat_control_long %>%
#   group_by(sector,start_year,funder,treated_both_funders) %>%
#   count() %>%
#   arrange(sector,start_year) %>%
#   print(n=300)

write.csv(dhs_treat_control_long,"./data/interim/dhs_treat_control_5k_annual.csv",row.names=FALSE)

dhs_treat_control_long %>%
  filter(funder!="control") %>% 
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  print(n=34)
# funder sector     n
# <chr>   <int> <int>
# 1 ch        998     2
# 2 ch        410     3
# 3 ch        530     3
# 4 ch        600     4
# 5 ch        330    31
# 6 ch        130    44
# 7 ch        920    58
# 8 ch        320    88
# 9 ch        420    93
# 10 ch        430   125
# 11 ch        520   128
# 12 ch        700   239
# 13 ch        140   436
# 14 ch        230   567
# 15 ch        220   742
# 16 ch        310   785
# 17 ch        160   797
# 18 wb        330   839
# 19 wb        410  1114
# 20 ch        110  1462
# 21 wb        240  1514
# 22 ch        210  1533
# 23 wb        220  1769
# 24 ch        150  2207
# 25 wb        320  3801
# 26 wb        230  4438
# 27 wb        110  5180
# 28 ch        120  5185
# 29 wb        120  5234
# 30 wb        310  6226
# 31 wb        160  7139
# 32 wb        140  8649
# 33 wb        210 11149
# 34 wb        150 14711

#generate contents of run script
dhs_treat_control_long %>%
  filter(funder!="control") %>% 
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(run=paste0("submit_and_wait call_CI_Conf_emb_5k_annual.slurm ",
                    funder,"_",sector," emb_5k_annual 1000 annual"
                    )) %>% 
  select(run) %>% 
  print (n=34)


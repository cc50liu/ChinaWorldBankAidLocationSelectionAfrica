# select_dhs_year_treat_control_ongoing.R
# Compare DHS cluster locations with project locations to select annual treatment and controls
# consider areas with ongoing projects as treated
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
             #consider ongoing projects "treated"
           transactions_start_year<=start_year & end_year>=start_year
             ) 

  if (nrow(sect_oda_df) > 0) {
    oda_sf <- sect_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,transactions_start_year,end_year,
             end_year_imputed,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector ", 
                                sector,", year ",start_year,": ",nrow(oda_sf)))
    
    #consider the project footprint to be the ADM2 in which it is hosted
    oda_buff_sf <- st_join(adm2_sf, oda_sf, join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, transactions_start_year, end_year, 
               end_year_imputed,geometry)
    
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
                                      transactions_start_year,end_year,
                                      end_year_imputed,geometry))

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
  #start_year = 2001
  
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
                               max_end_year = integer(),
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
          summarize(max_end_year = as.integer(max(end_year, na.rm = TRUE)),
                    proj_count = n(),
                    end_year_imputed = ifelse(any(end_year_imputed==TRUE),
                                              TRUE,
                                              FALSE)) %>%  
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
          summarize(max_end_year = as.integer(max(end_year, na.rm = TRUE)),
                  proj_count = n(),
                  end_year_imputed = ifelse(any(end_year_imputed==TRUE),
                                            TRUE,
                                            FALSE)) %>%  
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
wb_oda_df <- read.csv("./data/interim/africa_oda_sector_group_end.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
         funder=="WB") 
ch_oda_df <- read.csv("./data/interim/africa_oda_sector_group_end.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
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
#n=70965 (new projs) n=317,420 (ongoing projs)

#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector","funder","start_year","dhs_id","max_end_year","proj_count","end_year_imputed")

#identify places that had aid from both funders in a year - 3870 (new) 25926 (ongoing)
treated_both <- all_sectors %>%
  group_by(dhs_id, start_year, sector) %>%
  filter(any(funder == "wb") && any(funder == "ch")) %>%
  ungroup()

#remove the individual funder records when treated by both
all_sectors_less_both <- all_sectors %>% 
  anti_join(treated_both, by=c("dhs_id", "start_year", "sector"))

#prepare "both" rows to add back   
both_to_add <- treated_both %>% 
  group_by(dhs_id, start_year, sector) %>% 
  summarize(both_max_end_year=max(max_end_year),
            both_proj_count=sum(proj_count),
            both_end_year_imputed=ifelse(any(end_year_imputed==TRUE),
                                         TRUE,
                                         FALSE)) %>% 
  mutate(both_funder="both") %>% 
  ungroup() %>% 
  rename(max_end_year=both_max_end_year,
         proj_count=both_proj_count,
         funder=both_funder,
         end_year_imputed = both_end_year_imputed) %>% 
  select(!!!lapply(column_order,sym))

#add "both" rows and an additional image_group column
all_sectors_w_both <- rbind(all_sectors_less_both,both_to_add) 

# all_sectors_w_both <- all_sectors_w_both %>% 
#   mutate(image_group = case_when(
#     start_year %in% 1999:2001 ~ '1999:2001', 
#     start_year %in% 2002:2004 ~ '2002:2004', 
#     start_year %in% 2005:2007 ~ '2005:2007', 
#     start_year %in% 2008:2010 ~ '2008:2010', 
#     start_year %in% 2011:2013 ~ '2011:2013', 
#     start_year %in% 2014:2016 ~ '2014:2016'
#   ))

# image_group_v <- c('1999:2001', '2002:2004', '2005:2007', '2008:2010', '2011:2013', '2014:2016')

#generate all possible combinations of sector, start_year, and dhs_ids
expanded_dhs <- expand.grid(sector = sectors_v,
                           start_year = 2001:2014,
                           dhs_id = dhs_df$dhs_id) 

######## identify control points, excluding those treated by either funder during the year
control_points <- expanded_dhs %>% 
  anti_join(all_sectors_w_both, by=c("dhs_id", "start_year", "sector")) %>% 
  mutate(funder="control",
         max_end_year=NA_integer_,
         proj_count=0,
         end_year_imputed=NA) %>% 
  select(!!!lapply(column_order,sym))
#n=2,747,823

#n=3,052,280
dhs_treat_control_long <- rbind(all_sectors_w_both,control_points)

# dhs_treat_control_long %>% 
#   group_by(sector,image_group,funder) %>% 
#   count() %>% 
#   arrange(sector,image_group)

write.csv(dhs_treat_control_long,"./data/interim/dhs_treat_control_ongoing_long.csv",row.names=FALSE)
#dhs_treat_control_long <- read.csv("./data/interim/dhs_treat_control_ongoing_long.csv")

dhs_treat_control_long %>%
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  print(n=66)

# funder  sector      n
# <chr>    <int>  <int>
#   1 ch         998      4
# 2 ch         410      5
# 3 ch         530      6
# 4 both       320     39
# 5 ch         130     44
# 6 ch         920     58
# 7 ch         330     62
# 8 ch         420     93
# 9 ch         600     93
# 10 ch         430    125
# 11 ch         520    128
# 12 both       220    135
# 13 ch         700    239
# 14 both       140    407
# 15 both       230    512
# 16 both       310    518
# 17 ch         140    580
# 18 ch         310    640
# 19 ch         320    664
# 20 both       160   1000
# 21 ch         150   1010
# 22 ch         230   1074
# 23 ch         220   1359
# 24 ch         110   1379
# 25 ch         210   1386
# 26 ch         160   1429
# 27 both       110   1584
# 28 wb         330   1865
# 29 both       210   2690
# 30 both       150   2985
# 31 both       120   3093
# 32 wb         410   3240
# 33 ch         120   6111
# 34 wb         240   7279
# 35 wb         220   7783
# 36 wb         320  15252
# 37 wb         110  20226
# 38 wb         120  20883
# 39 wb         310  22253
# 40 wb         230  23812
# 41 wb         160  28015
# 42 wb         140  32030
# 43 wb         210  41678
# 44 wb         150  50689
# 45 control    150  84056
# 46 control    210  92986
# 47 control    140 105723
# 48 control    160 108296
# 49 control    120 108653
# 50 control    230 113342
# 51 control    310 115329
# 52 control    110 115551
# 53 control    320 122785
# 54 control    220 129463
# 55 control    240 131461
# 56 control    410 135495
# 57 control    330 136813
# 58 control    700 138501
# 59 control    520 138612
# 60 control    430 138615
# 61 control    420 138647
# 62 control    600 138647
# 63 control    920 138682
# 64 control    130 138696
# 65 control    530 138734
# 66 control    998 138736
# 

# select_dhs_treat_control_annual_s3.R
# Compare DHS cluster locations with project locations to select annual treatment and controls
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
  
  start_years <- 2002:2014

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

projection <- "ESRI:102023"

####################################################
#### Load administrative borders
adm2_borders <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- sf::st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
unique(sf::st_is_valid(adm2_borders))

####################################################
#### Process DHS survey points
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
#n=70965

#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector","funder","start_year","dhs_id","proj_count")

#identify places that had aid from both funders in a year - 3866
treated_both <- all_sectors %>%
  group_by(dhs_id, start_year, sector) %>% 
  filter(any(funder == "wb") && any(funder == "ch")) %>% 
  select(dhs_id, start_year, sector) %>% 
  ungroup() %>% 
  mutate(treated_both_funders = 1)

#add "both" column and an additional image_group column
all_sectors_both_column <- all_sectors %>% 
  left_join(treated_both, by=c("dhs_id", "start_year", "sector")) %>% 
  mutate(treated_both_funders = ifelse(is.na(treated_both_funders),0,1), 
         image_group = case_when(
          start_year %in% 2002:2004 ~ '2002:2004', 
          start_year %in% 2005:2007 ~ '2005:2007', 
          start_year %in% 2008:2010 ~ '2008:2010', 
          start_year %in% 2011:2013 ~ '2011:2013', 
          start_year %in% 2014:2016 ~ '2014:2016'
        ))

image_group_v <- c('2002:2004', '2005:2007', '2008:2010', '2011:2013', '2014:2016')

#generate all possible combinations of sector, image_group, and dhs_ids
expanded_dhs <- expand.grid(sector = sectors_v,
                           image_group = image_group_v,
                           dhs_id = dhs_df$dhs_id) %>% 
  mutate(image_group=as.character(image_group),
         treated_both_funders = 0)

######## identify control points, excluding those treated during the three year composite image
control_points <- expanded_dhs %>% 
  anti_join(all_sectors_both_column, by=c("dhs_id", "image_group", "sector")) %>% 
  mutate(funder="control",
         start_year=NA_integer_,
         proj_count=0) %>% 
  select(!!!lapply(column_order,sym), treated_both_funders,image_group)

#n=1107394
dhs_treat_control_long <- rbind(all_sectors_both_column,control_points)

# dhs_treat_control_long %>%
#   group_by(sector,image_group,funder,treated_both_funders) %>%
#   count() %>%
#   arrange(sector,image_group) %>% 
#   print(n=300)

write.csv(dhs_treat_control_long,"./data/interim/dhs_treat_control_annual_s3_both_2002.csv",row.names=FALSE)
#dhs_treat_control_long <- read.csv("./data/interim/ddhs_treat_control_annual_s3.csv")

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
# 13 ch        140   525
# 14 ch        230   569
# 15 ch        220   791
# 16 wb        330   839
# 17 ch        160   866
# 18 ch        310   883
# 19 wb        410  1114
# 20 wb        240  1514
# 21 ch        110  1581
# 22 wb        220  1818
# 23 ch        210  1950
# 24 ch        150  2978
# 25 wb        320  3801
# 26 wb        230  4440
# 27 wb        110  5299
# 28 ch        120  5504
# 29 wb        120  5553
# 30 wb        310  6324
# 31 wb        160  7208
# 32 wb        140  8738
# 33 wb        210 11566
# 34 wb        150 15482
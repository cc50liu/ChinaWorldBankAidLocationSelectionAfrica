# select_treat_control_group_annual3.R
# Compare DHS cluster locations with project locations to select annual treatment and controls
# consider areas with new projects as treated
# exclude image from being a control during 3-year period it is treated
# use sector groups instead of individual sectors
library(dplyr)
library(sf)
library(stringr)
library(tidyr)

rm(list=ls())

################################################################################
# Function preparing annual project footprints 
################################################################################
prep_projects <- function(oda_df, adm2_sf, projection, sector_group_param, start_year, debug_msg)
{
  #uncomment for testing 
  # oda_df <- ch_oda_df
  # sector_group <- "SIS"
  # adm2_sf <- adm2_borders
  
  sect_group_oda_df  <- oda_df %>%
    filter(sector_group==sector_group_param &
             #consider new projects "treated"
             transactions_start_year==start_year
    ) 
  
  if (nrow(sect_group_oda_df) > 0) {
    oda_sf <- sect_group_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,transactions_start_year,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector group ", 
                                sector_group_param,", year ",start_year,": ",nrow(oda_sf)))
    
    #consider the project footprint to be the ADM2 in which it is hosted
    oda_buff_sf <- st_join(adm2_sf, oda_sf, join = st_intersects, left = FALSE) %>%
      select(geoname_id, precision_code, transactions_start_year,geometry)
    
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
                                      transactions_start_year,geometry))
        
      }
    }
    
  } else {
    #create empty sf object
    oda_buff_sf <- sf::st_sf(geometry = sf::st_sfc())
    print(paste0("No project locations for sector_group ",sector_group_param,", year ",start_year))
  } 
  
  return(oda_buff_sf)
}


################################################################################
# Function that spatially intersects projects and dhs points per sector group,
# for study years
################################################################################
process_sector_groups <- function(sector_group_param, projection, dhs_buff_sf,
                                  wb_oda_df, ch_oda_df,adm2_borders, debug_msg) 
{
  #uncomment to test
  #sector_group_param <- "SIS"
  #dhs_buff_sf <- dhs_sf
  #start_year = 2001
  
  start_years <- 2001:2014
  
  sector_group_output <- lapply(start_years, function(start_year) {
    #uncomment to test
    #start_year = 2002
    
    ####################################################
    #### Prepare project location data for the year
    if (exists("wb_oda_df", inherits=TRUE)) {
      wb_oda_sect_buff_sf <- prep_projects(wb_oda_df,adm2_borders, projection, 
                                           sector_group_param, start_year, debug_msg)
      
      if (debug_msg & (nrow(wb_oda_sect_buff_sf) > 0))
        print(paste0("Locations in wb_oda_sect_buff_sf for sector group ", 
                     sector_group_param,", year ",start_year,": ",nrow(wb_oda_sect_buff_sf)))
    } else if (debug_msg) {
      print(paste("No WB locations for sector",sector))
    }
    
    if (exists("ch_oda_df", inherits=TRUE)) {
      ch_oda_sect_buff_sf <- prep_projects(ch_oda_df,adm2_borders, projection, 
                                           sector_group_param, start_year, debug_msg)
      
      if (debug_msg & (nrow(ch_oda_sect_buff_sf) > 0))
        print(paste0("Locations in ch_oda_sect_buff_sf for sector group", 
                     sector_group_param,", year ",start_year,": ",nrow(ch_oda_sect_buff_sf)))
    } else if (debug_msg) {
      print(paste("No CH locations for sector group and year ",sector_group_param,start_year))
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
        print(paste("No wB projects intersect with DHS points for sector group/year ",
                    sector_group,start_year))
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
        print(paste("No CH projects intersect with DHS points for sector group/year ",
                    sector_group_param,start_year))
      } 
    }
    
    return(rbind(return_wb_df,return_ch_df))
  })
  
  all_years_funders <- do.call(rbind,sector_group_output)
  return(cbind(sector_group_param,all_years_funders))
}



################################################################################
# main code starts here
################################################################################
wb_oda_df <- read.csv("./data/interim/africa_oda_sector_group_v2.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
           funder=="WB") 
ch_oda_df <- read.csv("./data/interim/africa_oda_sector_group_v2.csv") %>% 
  filter(precision_code %in% c(1,2,3) &  #Exact, near, ADM2
           funder=="CH") 

africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
debug_msg=as.logical(TRUE)

#generate vectors of countries and sectors to pass to function
dhs_isos_v <- read.csv("./data/interim/dhs_est_iwi.csv") %>%
  select(iso3) %>%
  distinct()  %>%
  pull(iso3)

sector_groups_v <- unique(rbind(
  wb_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(sector_group),
  ch_oda_df %>% filter(site_iso3 %in% dhs_isos_v) %>% distinct(sector_group))) %>%
  arrange(sector_group) %>%
  pull(sector_group)

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
output <- lapply(sector_groups_v, function(sector_group) {
  process_sector_groups(sector_group, projection, dhs_sf, wb_oda_df,
                  ch_oda_df, adm2_borders, debug_msg)
})

all_sector_groups <- do.call(rbind,output) %>% 
  rename("sector_group" = "sector_group_param")
#n=60362 

#use this variable to ensure a consistent column ordering working with the data
column_order <- c("sector_group","funder","start_year","dhs_id","proj_count")

#identify places that had aid from both funders in a year - 3870 (new) 25926 (ongoing)
treated_both <- all_sector_groups %>%
  group_by(dhs_id, start_year, sector_group) %>%
  filter(any(funder == "wb") && any(funder == "ch")) %>%
  ungroup()

#remove the individual funder records when treated by both
all_sectors_less_both <- all_sector_groups %>% 
  anti_join(treated_both, by=c("dhs_id", "start_year", "sector_group"))

#prepare "both" rows to add back   
both_to_add <- treated_both %>% 
  group_by(dhs_id, start_year, sector_group) %>% 
  summarize(both_proj_count=sum(proj_count)) %>% 
  mutate(both_funder="both") %>% 
  ungroup() %>% 
  rename(proj_count=both_proj_count,
         funder=both_funder) %>% 
  select(!!!lapply(column_order,sym))

#add "both" rows and an additional image_group column
all_sectors_w_both <- rbind(all_sectors_less_both,both_to_add) 

all_sectors_w_both <- all_sectors_w_both %>%
  mutate(image_group = case_when(
    start_year %in% 1999:2001 ~ '1999:2001',
    start_year %in% 2002:2004 ~ '2002:2004',
    start_year %in% 2005:2007 ~ '2005:2007',
    start_year %in% 2008:2010 ~ '2008:2010',
    start_year %in% 2011:2013 ~ '2011:2013',
    start_year %in% 2014:2016 ~ '2014:2016'
  ))

image_group_v <- c('1999:2001', '2002:2004', '2005:2007', '2008:2010', '2011:2013', '2014:2016')

#generate all possible combinations of sector_group, image_group, and dhs_ids
expanded_dhs <- expand.grid(sector_group = sector_groups_v,
                           image_group = image_group_v,
                           dhs_id = dhs_df$dhs_id) %>% 
  mutate(image_group=as.character(image_group))

######## identify control points, excluding those treated during the three year composite image
control_points <- expanded_dhs %>% 
  anti_join(all_sectors_w_both, by=c("dhs_id", "image_group", "sector_group")) %>% 
  mutate(funder="control",
         start_year=NA_integer_,
         proj_count=0) %>% 
  select(!!!lapply(column_order,sym), image_group)
#n=254,311

dhs_treat_control_long <- rbind(all_sectors_w_both,control_points)

dhs_treat_control_long %>%
  group_by(sector_group,image_group,funder) %>%
  count() %>%
  arrange(sector_group,image_group) %>% 
  print(n=95)

write.csv(dhs_treat_control_long,"./data/interim/dhs_treat_control_group_annual3.csv",row.names=FALSE)

dhs_treat_control_long %>%
  group_by(funder,sector_group) %>%
  count() %>%
  arrange(n) %>%
  print(n=17)

# funder  sector_group     n
# <chr>   <chr>        <int>
# 1 both    PRO             98
# 2 ch      OTH            223
# 3 ch      DIR            521
# 4 both    EIS            856
# 5 ch      PRO            863
# 6 wb      OTH           1114
# 7 ch      EIS           1915
# 8 both    SIS           3145
# 9 ch      SIS           5235
# 10 wb      PRO           7960
# 11 wb      EIS          16141
# 12 wb      SIS          18192
# 13 control SIS          40715
# 14 control EIS          44779
# 15 control PRO          51634
# 16 control OTH          58244
# 17 control DIR          58939

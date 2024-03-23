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
####################################################
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")

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
write.csv(all_sectors,"./data/interim/all_sectors.csv",row.names=FALSE)

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
write.csv(all_sectors_expanded_df,"./data/interim/dhs_treated_sector_annual.csv",row.names=FALSE)

#####################################
# annual records
#####################################
#generate commands to submit slurm scripts
all_sectors_expanded_df %>%
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(run=paste0("sbatch call_CI_Conf_emb_5k_annual.slurm ",
                    funder,"_",sector," emb_5k_annual 1000 annual"
                    )) %>% 
  select(run) %>% 
  print (n=34)

# 1 sbatch call_CI_Conf_emb_5k_annual.slurm ch_430 emb_5k_annual 1000 annual
# 2 sbatch call_CI_Conf_emb_5k_annual.slurm ch_520 emb_5k_annual 1000 annual
# 3 sbatch call_CI_Conf_emb_5k_annual.slurm ch_700 emb_5k_annual 1000 annual
# 4 sbatch call_CI_Conf_emb_5k_annual.slurm ch_140 emb_5k_annual 1000 annual
# 5 sbatch call_CI_Conf_emb_5k_annual.slurm ch_230 emb_5k_annual 1000 annual
# 6 sbatch call_CI_Conf_emb_5k_annual.slurm ch_220 emb_5k_annual 1000 annual
# 7 sbatch call_CI_Conf_emb_5k_annual.slurm ch_310 emb_5k_annual 1000 annual
# 8 sbatch call_CI_Conf_emb_5k_annual.slurm ch_160 emb_5k_annual 1000 annual
# 9 sbatch call_CI_Conf_emb_5k_annual.slurm wb_330 emb_5k_annual 1000 annual
# 10 sbatch call_CI_Conf_emb_5k_annual.slurm wb_410 emb_5k_annual 1000 annual
# 11 sbatch call_CI_Conf_emb_5k_annual.slurm ch_110 emb_5k_annual 1000 annual
# 12 sbatch call_CI_Conf_emb_5k_annual.slurm wb_240 emb_5k_annual 1000 annual
# 13 sbatch call_CI_Conf_emb_5k_annual.slurm ch_210 emb_5k_annual 1000 annual
# 14 sbatch call_CI_Conf_emb_5k_annual.slurm wb_220 emb_5k_annual 1000 annual
# 15 sbatch call_CI_Conf_emb_5k_annual.slurm ch_150 emb_5k_annual 1000 annual
# 16 sbatch call_CI_Conf_emb_5k_annual.slurm wb_320 emb_5k_annual 1000 annual
# 17 sbatch call_CI_Conf_emb_5k_annual.slurm wb_230 emb_5k_annual 1000 annual
# 18 sbatch call_CI_Conf_emb_5k_annual.slurm wb_110 emb_5k_annual 1000 annual
# 19 sbatch call_CI_Conf_emb_5k_annual.slurm ch_120 emb_5k_annual 1000 annual
# 20 sbatch call_CI_Conf_emb_5k_annual.slurm wb_120 emb_5k_annual 1000 annual
# 21 sbatch call_CI_Conf_emb_5k_annual.slurm wb_310 emb_5k_annual 1000 annual
# 22 sbatch call_CI_Conf_emb_5k_annual.slurm wb_160 emb_5k_annual 1000 annual
# 23 sbatch call_CI_Conf_emb_5k_annual.slurm wb_140 emb_5k_annual 1000 annual
# 24 sbatch call_CI_Conf_emb_5k_annual.slurm wb_210 emb_5k_annual 1000 annual
# 25 sbatch call_CI_Conf_emb_5k_annual.slurm wb_150 emb_5k_annual 1000 annual

#####################################
# year group records
#####################################
treated_year_group <- all_sectors_expanded_df %>% 
  group_by(sector, funder, dhs_id, year_group) %>% 
  summarize(sum_proj_count = sum(proj_count, na.rm=TRUE)) %>% 
  rename(proj_count = sum_proj_count) %>% 
  ungroup()
#n=76256

write.csv(treated_year_group,"./data/interim/dhs_treated_sector_3yr.csv",row.names=FALSE)

#generate commands to submit slurm scripts
treated_year_group %>%
  group_by(funder,sector) %>%
  count() %>%
  arrange(n) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(run=paste0("sbatch call_CI_Conf_emb_5k_3yr.slurm ",
                    funder,"_",sector," emb_5k_3yr 1000 3yr"
  )) %>% 
  select(run) %>% 
  print (n=100)

# 1 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_430 emb_5k_3yr 1000 3yr
# 2 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_520 emb_5k_3yr 1000 3yr
# 3 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_700 emb_5k_3yr 1000 3yr
# 4 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_140 emb_5k_3yr 1000 3yr
# 5 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_230 emb_5k_3yr 1000 3yr
# 6 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_220 emb_5k_3yr 1000 3yr
# 7 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_310 emb_5k_3yr 1000 3yr
# 8 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_160 emb_5k_3yr 1000 3yr
# 9 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_330 emb_5k_3yr 1000 3yr
# 10 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_410 emb_5k_3yr 1000 3yr
# 11 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_110 emb_5k_3yr 1000 3yr
# 12 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_210 emb_5k_3yr 1000 3yr
# 13 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_240 emb_5k_3yr 1000 3yr
# 14 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_220 emb_5k_3yr 1000 3yr
# 15 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_150 emb_5k_3yr 1000 3yr
# 16 sbatch call_CI_Conf_emb_5k_3yr.slurm ch_120 emb_5k_3yr 1000 3yr
# 17 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_320 emb_5k_3yr 1000 3yr
# 18 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_230 emb_5k_3yr 1000 3yr
# 19 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_110 emb_5k_3yr 1000 3yr
# 20 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_120 emb_5k_3yr 1000 3yr
# 21 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_310 emb_5k_3yr 1000 3yr
# 22 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_160 emb_5k_3yr 1000 3yr
# 23 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_140 emb_5k_3yr 1000 3yr
# 24 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_210 emb_5k_3yr 1000 3yr
# 25 sbatch call_CI_Conf_emb_5k_3yr.slurm wb_150 emb_5k_3yr 1000 3yr

#####################################
# sector group annual records
#####################################
treated_sector_group_annual <- all_sectors_expanded_df %>% 
  group_by(sector_group, funder, dhs_id, start_year) %>% 
  summarize(sum_proj_count = sum(proj_count, na.rm=TRUE)) %>% 
  rename(proj_count = sum_proj_count) %>% 
  ungroup()
#n=57628

write.csv(treated_sector_group_annual,"./data/interim/dhs_treated_sector_group_annual.csv",row.names=FALSE)

#generate commands to submit slurm scripts
treated_sector_group_annual %>%
  group_by(funder,sector_group) %>%
  count() %>%
  arrange(n) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(run=paste0("sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ",
                    funder,"_",sector_group," emb_5k_sgroup_annual 1000 annual"
  )) %>% 
  select(run) %>% 
  print (n=100)
# 1 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ch_OTH emb_5k_sgroup_annual 1000 annual
# 2 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ch_DIR emb_5k_sgroup_annual 1000 annual
# 3 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ch_PRO emb_5k_sgroup_annual 1000 annual
# 4 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm wb_OTH emb_5k_sgroup_annual 1000 annual
# 5 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ch_EIS emb_5k_sgroup_annual 1000 annual
# 6 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm wb_PRO emb_5k_sgroup_annual 1000 annual
# 7 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm ch_SIS emb_5k_sgroup_annual 1000 annual
# 8 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm wb_EIS emb_5k_sgroup_annual 1000 annual
# 9 sbatch call_CI_Conf_emb_5k_sgroup_annual.slurm wb_SIS emb_5k_sgroup_annual 1000 annual

#####################################
# sector group 3yr records
#####################################
treated_sgroup_3yr <- all_sectors_expanded_df %>% 
  group_by(sector_group, funder, dhs_id, year_group) %>% 
  summarize(sum_proj_count = sum(proj_count, na.rm=TRUE)) %>% 
  rename(proj_count = sum_proj_count) %>% 
  ungroup()
#n=45150

write.csv(treated_sector_group_3yr,"./data/interim/dhs_treated_sector_group_3yr.csv",row.names=FALSE)

#generate commands to submit slurm scripts
treated_sector_group_3yr %>%
  group_by(funder,sector_group) %>%
  count() %>%
  arrange(n) %>%
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(run=paste0("sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ",
                    funder,"_",sector_group," emb_5k_sgroup_3yr 1000 3yr"
  )) %>% 
  select(run) %>% 
  print (n=100)
# 1 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ch_OTH emb_5k_sgroup_3yr 1000 3yr
# 2 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ch_DIR emb_5k_sgroup_3yr 1000 3yr
# 3 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ch_PRO emb_5k_sgroup_3yr 1000 3yr
# 4 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm wb_OTH emb_5k_sgroup_3yr 1000 3yr
# 5 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ch_EIS emb_5k_sgroup_3yr 1000 3yr
# 6 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm ch_SIS emb_5k_sgroup_3yr 1000 3yr
# 7 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm wb_PRO emb_5k_sgroup_3yr 1000 3yr
# 8 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm wb_EIS emb_5k_sgroup_3yr 1000 3yr
# 9 sbatch call_CI_Conf_emb_5k_sgroup_3yr.slurm wb_SIS emb_5k_sgroup_3yr 1000 3yr
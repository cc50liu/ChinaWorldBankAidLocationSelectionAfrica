# select_dhs_year_treat_control_sectorAfrica
# Compare DHS cluster locations with project locations to select treatment and controls
# and min/max project years
library(dplyr)
library(sf)
library(stringr)

rm(list=ls())

################################################################################
prep_projects <- function(oda_df, adm2_sf, projection, sector, debug_msg)
{
  #uncomment for testing 
  # oda_df <- ch_oda_df
  # sector <- 110
  # adm2_sf <- adm2_borders
  
  sect_oda_df  <- oda_df %>%
    filter(ad_sector_codes==sector) 

  #adding year changes from 142 to 162 observations
  if (nrow(sect_oda_df) > 0) {
    oda_sf <- sect_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,transactions_start_year,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector ", 
                                sector,": ",nrow(oda_sf),
                                " By precision p1:",nrow(oda_sf[oda_sf$precision_code==1,]),
                                " p2:",nrow(oda_sf[oda_sf$precision_code==2,]),
                                " p3:",nrow(oda_sf[oda_sf$precision_code==3,])
    ))
    
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
                               select(geoname_id,precision_code,transactions_start_year,geometry))

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
# Determine dhs treatments/control status by sector
################################################################################
process_sectors <- function(sector, projection, dhs_buff_sf,wb_oda_df, ch_oda_df,
                            adm2_borders, debug_msg) 
{
  #uncomment to test
  #sector <- 110

  ####################################################
  #### Prepare project location data 
  if (exists("wb_oda_df", inherits=TRUE)) {
    wb_oda_sect_buff_sf <- prep_projects(wb_oda_df,adm2_borders, projection, 
                                         sector, debug_msg)

    if (debug_msg & (nrow(wb_oda_sect_buff_sf) > 0))
      print(paste0("Locations in wb_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(wb_oda_sect_buff_sf),
                            " By precision p1:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==1,]),
                            " p2:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==2,]),
                            " p3:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==3,]))
                            )
  } else if (debug_msg) {
    print(paste("No WB locations for sector",sector))
  }
  
  if (exists("ch_oda_df", inherits=TRUE)) {
    ch_oda_sect_buff_sf <- prep_projects(ch_oda_df,adm2_borders, projection, 
                                         sector, debug_msg)
    
    if (debug_msg & (nrow(ch_oda_sect_buff_sf) > 0))
      print(paste0("Locations in ch_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(ch_oda_sect_buff_sf),
                            " By precision p1:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==1,]),
                            " p2:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==2,]),
                            " p3:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==3,]))
    )
  } else if (debug_msg) {
    print(paste("No CH locations for sector",sector))
  }
  
  ############################################################
  # Identify treatment and control DHS points
  ############################################################
  #Treated points intersect with at least one project
  #WB precision 1
  if (nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==1,]) > 0) {
    wb1_df <- st_join(dhs_buff_sf,
                        wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==1,],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
    
  } else {
    wb1_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  }
  #WB precision 2
  if (nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==2,]) > 0) {
    wb2_df <- st_join(dhs_buff_sf,
                      wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==2,],
                      left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    wb2_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  } 
  #WB precision 3
  if (nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==3,]) > 0) {
    wb3_df <- st_join(dhs_buff_sf,
                      wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==3,],
                      left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    wb3_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  }  
  #CH precision 1  
  if (nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==1,]) > 0) {  
    ch1_df <- st_join(dhs_buff_sf,
                        ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==1,],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    ch1_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  }
  #CH precision 2  
  if (nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==2,]) > 0) {  
    ch2_df <- st_join(dhs_buff_sf,
                      ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==2,],
                      left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    ch2_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  }  
  #CH precision 3  
  if (nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==3,]) > 0) {  
    ch3_df <- st_join(dhs_buff_sf,
                      ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==3,],
                      left=FALSE) %>%
      st_drop_geometry() %>%
      group_by(dhs_id) %>%
      summarize(min_transactions_start_year = min(transactions_start_year, na.rm = TRUE)) %>% 
      ungroup()
  } else {
    ch3_df <- data.frame(dhs_id = integer(),min_transactions_start_year = integer())
  }

  #create sector-level dataframes also
  wb_df <- bind_rows(wb1_df, wb2_df, wb3_df) %>% 
    group_by(dhs_id) %>% 
    summarize(min_oda_year=min(min_transactions_start_year,na.rm=TRUE)) 
  
  ch_df <- bind_rows(ch1_df, ch2_df, ch3_df) %>% 
    group_by(dhs_id) %>% 
    summarize(min_oda_year=min(min_transactions_start_year,na.rm=TRUE))
  
  return(setNames(list(wb1_df, ch1_df, wb2_df, ch2_df, wb3_df, ch3_df, wb_df, ch_df), 
                  c(paste0("wb_", sector,"_p1"), 
                    paste0("ch_", sector,"_p1"),
                    paste0("wb_", sector,"_p2"), 
                    paste0("ch_", sector,"_p2"),
                    paste0("wb_", sector,"_p3"), 
                    paste0("ch_", sector,"_p3"),
                    paste0("wb_", sector),
                    paste0("ch_", sector)
                  ))
         )
         
}

###############################################################
## main code starts here
wb_oda_df <- read.csv("./data/interim/wb_africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,2,3)) #Exact, near, ADM2
ch_oda_df <- read.csv("./data/interim/ch_africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,2,3)) #Exact, near, ADM2

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

#don't filter, but include all sectors that had projects (23)
            # %>%
            # filter(ad_sector_codes %in% 
            #        c(#Social Infrastructure & Services
            #          110,  #Education
            #          120,  #Health
            #          130,  # Population Policies / Programmes and Reproductive Health
            #          140,  # Water Supply and Sanitation
            #          150,  #Government and Civil Society
            #          160,  #Other Social Infrastructure and Services
            #          #Economic Infrastructure & Services 
            #          210,  #Transport and Storage
            #          220,  # Communications
            #          230,  # Energy Generation and Supply
            #          240,  # Banking and Financial Services
            #          250,  # Business and Other Services
            #          # Production     
            #          310,  #Agriculture, Forestry and Fishing
            #          320,  #Industry, Mining, Construction
            #          330  #Trade and Tourism
            #          ))  



projection <- "ESRI:102023"

####################################################
#### Load administrative borders
adm2_borders <- sf::st_read("./data/country_regions/gadm2_clean.shp")  %>%
  filter(ISO %in% africa_isos_df$iso3)
sf::st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
unique(sf::st_is_valid(adm2_borders))

####################################################
#### Process DHS survey points
dhs_df <- read.csv("./data/interim/dhs_est_iwi.csv")

dhs_sf <- dhs_df  %>%
  st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,year,iso3,rural,geometry)  

if (exists("dhs_sf", inherits=TRUE)) {
  dhs_sf <- st_transform(dhs_sf,crs=st_crs(projection))
  if (debug_msg) print(paste("Obs in dhs_sf:",nrow(dhs_sf)))
  
  #put a 2k or 5k buffer around DHS points, according to rural/urban privacy offset
  dhs_buff_sf <- rbind(
    st_buffer(dhs_sf[dhs_sf$rural==1,], 5000,  #meter units
              endCapStyle = "ROUND"),
    st_buffer(dhs_sf[dhs_sf$rural==0,], 2000,  #meter units
              endCapStyle = "ROUND")
  )
}

####################################################
output <- lapply(sectors_v, function(sector) {
  process_sectors(sector, projection, dhs_buff_sf, wb_oda_df,
                  ch_oda_df, adm2_borders, debug_msg)
})

dhs_sect_prec_df <- dhs_df
dhs_sect_df <- dhs_df
#iterate over the output, adding columns to 
# dhs_sect_prec_df for each funder, sector, and precision with values 1=treated, 0=not treated
# dhs_sect_df for each funder/sector with values 1=treated, 0=control, -1=treated by other funder
for (i in 1:length(output)) {
  #get the names for this sector
  #i=1 #uncomment to test
  list_names <- names(output[[i]])
  sector <- unique(sub(".*_(\\d+).*", "\\1", list_names))
  for (list_name_ in list_names) {
    #list_name_ <- "wb_110" #uncomment to test
    df <- output[[i]][[list_name_]]
    print(paste(list_name_,"has",nrow(df),"treatments"))
    if (grepl("^(wb|ch)_\\d+_p\\d+$", list_name_)) {
      #add detailed precision columns to dhs_sect_prec_df
      dhs_sect_prec_df[[list_name_]] <- as.integer(dhs_sect_prec_df$dhs_id %in% df$dhs_id)
      #join to get earliest transaction year
      dhs_sect_prec_df <- dhs_sect_prec_df %>% 
        left_join(df,by="dhs_id") %>% 
        rename_with(~ paste0(list_name_,"_min_oda_year"), min_transactions_start_year)
    } else {
      #add sector summary columns to dhs_sect_df
      dhs_sect_df[[list_name_]] <- as.integer(dhs_sect_df$dhs_id %in% df$dhs_id)
      #join to get earliest transaction year
      dhs_sect_df <- dhs_sect_df %>% 
        left_join(df,by="dhs_id") %>% 
        rename_with(~ paste0(list_name_,"_min_oda_year"), min_oda_year)
    }
  }
  #adjust for the other funder:
  # set values of 0 (control) to -1 if the other funder did a treatment there
  # set values of 1 (treatment) to 2 if the other funder also did a treatment there
  wb_colname <- paste0("wb_",sector)
  ch_colname <- paste0("ch_",sector)
  dhs_sect_df <- dhs_sect_df %>% 
    mutate(!!wb_colname := ifelse(get(wb_colname) == 0 & get(ch_colname) == 1, -1, get(wb_colname)),
           !!ch_colname := ifelse(get(ch_colname) == 0 & get(wb_colname) == 1, -1, get(ch_colname)),
           !!wb_colname := ifelse(get(wb_colname) == 1 & get(ch_colname) == 1,  2, get(wb_colname)),
           !!ch_colname := ifelse(get(ch_colname) == 1 & get(wb_colname) == 1,  2, get(ch_colname))
           )
  
}

write.csv(dhs_sect_prec_df,"./data/interim/dhs_treat_control_sect_prec_year.csv",row.names=FALSE)
write.csv(dhs_sect_df,"./data/interim/dhs_treat_control_sector_year.csv",row.names=FALSE)





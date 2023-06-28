# Compare DHS cluster locations with project locations to select treatment and controls
library(dplyr)
library(sf)
library(tmap)
library(stringr)
tmap_options(check.and.fix = TRUE)

rm(list=ls())

################################################################################
# Function to generate and save validation maps
################################################################################
gen_save_validation_maps <- function(funder, max_precision,adm0_sf,adm1_sf,adm2_sf,plad_sf,
                                     dhs_sf,aid_sf,sector,sector_name,debug_msg)
  
{
  #uncomment to test
  # funder <- "CH"
  # max_precision <- 3
  # plad_sf <- plad_africa_adm1_sf
  # adm0_sf <- country_borders
  # adm2_sf <- adm2_borders
  # dhs_sf <- dhs_buff_sf
  # aid_sf <- ch_oda_sect_buff_sf
  
  
  if (as.logical(debug_msg)) {
    print(paste("Generating validation map for funder:",funder,"max_precision:",
                max_precision,"sector_name:",sector_name))
  }
  
  proj_map <- tm_shape(adm0_sf) +
    tm_borders(lwd=2) +
    # tm_shape(plad_sf) +
    # tm_borders(col="yellow",lwd=3) +
    # tm_shape(adm1_sf) +
    # tm_borders() +
    # tm_shape(shp=dhs_sf) +
    # tm_dots(size=.5, col="gray", alpha=.3) +
    tm_layout(main.title.size=1,
              main.title = paste0(toupper(funder)," Aid (precision<=", max_precision,")\n Sector: ",
                                 sector,":",sector_name),
              main.title.position=c("center","top")) +
    tm_add_legend(type = "fill"
                  , col = c("lightblue", "pink")
                  , labels = c("ADM1 aid (p4)", "ADM2 aid (p1&3)"))  +
    tm_layout(legend.position = c("right", "bottom"),
              legend.text.size = 1.4,
              frame = F ,
              legend.outside = T, 
              outer.margins = c(0, 0, 0, 0), 
              legend.outside.size = .25
    )
  
  if (max_precision >= 4 &
      nrow(aid_sf[aid_sf$precision_code==4,]) > 0) {
    proj_map <- proj_map +
      tm_shape(shp=aid_sf[aid_sf$precision_code==4,]) +
      tm_borders() +
      tm_fill(col="lightblue") 
  }

  if (max_precision >= 3) {
    #show ADM2 borders for projects at that level and precision1 projects that successfuly
    #joined to ADM2 level
    if (nrow(aid_sf[(aid_sf$precision_code==3 | aid_sf$precision_code==1) & 
                    aid_sf$geometry_type != "POINT",]) > 0) {
      proj_map <- proj_map +
        tm_shape(shp=aid_sf[(aid_sf$precision_code==3 | aid_sf$precision_code==1) & 
                              aid_sf$geometry_type != "POINT",]) +
        tm_borders() +
        tm_fill(col="pink")       
    }
    
    #show points for projects that failed to join to ADM2 level 
    if (nrow(aid_sf[(aid_sf$precision_code==3 | aid_sf$precision_code==1) & 
                    aid_sf$geometry_type == "POINT",]) > 0) {
      proj_map <- proj_map +
        tm_shape(shp=aid_sf[(aid_sf$precision_code==3 | aid_sf$precision_code==1) & 
                              aid_sf$geometry_type == "POINT",]) +
        tm_dots(size=.5,col="red",alpha=.3)     
    }
  }  

  proj_map
  
  tmap_save(proj_map,paste0("./figures/",funder,"_p",max_precision,
                            "_s",sector,".png"))
  
}



################################################################################
prep_projects <- function(oda_df, adm1_sf, adm2_sf, projection, sector, debug_msg)
{
  #uncomment for testing 
  # oda_df <- ch_oda_df
  # sector <- 110
  # adm1_sf <- adm1_borders
  # adm2_sf <- adm2_borders
  
  sect_oda_df  <- oda_df %>%
    filter(precision_code %in% c(1,3,4) & 
             ad_sector_codes==sector) 

  if (nrow(sect_oda_df) > 0) {
    oda_sf <- sect_oda_df %>% 
      st_as_sf(coords = c("longitude", "latitude"),crs="EPSG:4326") %>% 
      select(geoname_id,precision_code,geometry) %>%
      dplyr::distinct()    
    oda_sf <- st_transform(oda_sf,crs=st_crs(projection))
    # st_is_valid(oda_sf)
    # st_is_empty(oda_sf)
    if (debug_msg) print(paste0("Locations in oda_sf for sector ", 
                                sector,": ",nrow(oda_sf),
                                " By precision p1:",nrow(oda_sf[oda_sf$precision_code==1,]),
                                " p3:",nrow(oda_sf[oda_sf$precision_code==3,]),
                                " p4:",nrow(oda_sf[oda_sf$precision_code==4,])
    ))
    
    #put buffers around projects based on their precision codes
    # 1: exact, buffer to adm2 level
    # 2: within 25k, exclude due to uncertain precision
    # 3: adm2, project covers the boundaries of the containing adm2
    # 4: adm1, project covers the boundaries of the containing adm1
    oda_buff_sf <- rbind(
      st_join(adm2_sf, oda_sf[oda_sf$precision_code==1, ], join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, geometry),
      st_join(adm2_sf, oda_sf[oda_sf$precision_code==3, ], join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, geometry),    
      st_join(adm1_sf, oda_sf[oda_sf$precision_code==4,], join = st_intersects, left = FALSE) %>%
        select(geoname_id, precision_code, geometry)
    )
    
    #some locations are outside official ADM2 borders, find nearest
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
                               select(geoname_id,precision_code,geometry))
      }
    }

  } else {
    #create empty sf object
    oda_buff_sf <- sf::st_sf(geometry = sf::st_sfc())
    print(paste("No project locations for sector",sector))
  } 
  
  #get the geometry type so mapping code can deal with projects that failed to join to ADM2 level
  oda_buff_sf <- oda_buff_sf %>% 
    mutate(geometry_type = st_geometry_type(.))
  
  return(oda_buff_sf)
}


################################################################################
# Function to generate maps and process treatments/controls for each sector
################################################################################
process_sectors <- function(sector, sector_name, projection,  
                            dhs_buff_sf,wb_oda_df, ch_oda_df,
                            country_borders, adm1_borders, adm2_borders, plad_africa_adm1_sf,
                            debug_msg) 
{
  #uncomment to test
  # sector <- 110
  # sector_name <- "Education"
  # projection <- "ESRI:102023"

  ####################################################
  #### Prepare project location data 
  if (exists("wb_oda_df", inherits=TRUE)) {
    wb_oda_sect_buff_sf <- prep_projects(wb_oda_df,adm1_borders,
                                         adm2_borders, projection, sector, debug_msg)

    if (debug_msg & (nrow(wb_oda_sect_buff_sf) > 0))
      print(paste0("Locations in wb_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(wb_oda_sect_buff_sf),
                            " By precision p1:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==1,]),
                            " p3:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==3,]),
                            " p4:",nrow(wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==4,])
                            ))
  } else if (debug_msg) {
    print(paste("No WB locations for sector",sector))
  }
  
  if (exists("ch_oda_df", inherits=TRUE)) {
    ch_oda_sect_buff_sf <- prep_projects(ch_oda_df,adm1_borders,
                                         adm2_borders, projection, sector, debug_msg)
    
    if (debug_msg & (nrow(ch_oda_sect_buff_sf) > 0))
      print(paste0("Locations in ch_oda_sect_buff_sf for sector ", 
                            sector,": ",nrow(ch_oda_sect_buff_sf),
                            " By precision p1:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==1,]),
                            " p3:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==3,]),
                            " p4:",nrow(ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==4,])
    ))
  } else if (debug_msg) {
    print(paste("No CH locations for sector",sector))
  }
  
  ####################################################
  #### Generate and save validation maps
  ####################################################
  if (nrow(wb_oda_sect_buff_sf) > 0) {
    gen_save_validation_maps("WB",3,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             wb_oda_sect_buff_sf,sector,sector_name,debug_msg)
    gen_save_validation_maps("WB",4,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             wb_oda_sect_buff_sf,sector,sector_name,debug_msg)
  }
  if (nrow(ch_oda_sect_buff_sf) > 0) {
    gen_save_validation_maps("CH",3,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             ch_oda_sect_buff_sf,sector,sector_name,debug_msg)
    gen_save_validation_maps("CH",4,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             ch_oda_sect_buff_sf,sector,sector_name,debug_msg)
  }
  if (nrow(wb_oda_sect_buff_sf) > 0 &
      nrow(ch_oda_sect_buff_sf) > 0)  {    
    gen_save_validation_maps("BOTH",3,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             rbind(ch_oda_sect_buff_sf,wb_oda_sect_buff_sf),
                             sector,sector_name,debug_msg)
    gen_save_validation_maps("BOTH",4,country_borders,adm1_borders,
                             adm2_borders,plad_africa_adm1_sf,dhs_buff_sf,
                             rbind(ch_oda_sect_buff_sf,wb_oda_sect_buff_sf),
                             sector,sector_name,debug_msg)
  }
  
  ############################################################
  # Identify treatment and control DHS points
  ############################################################
  #Treated points intersect with at least one project
  if (nrow(wb_oda_sect_buff_sf) > 0) {
    wb_t3_df <- st_join(dhs_buff_sf,
                        wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code <= 3, ],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      distinct(dhs_id)
    
    wb_t4_df <- st_join(dhs_buff_sf,
                        wb_oda_sect_buff_sf[wb_oda_sect_buff_sf$precision_code==4, ],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      distinct(dhs_id)
  } else {
    #create empty dataframes for use in rbind below
    wb_t3_df <- data.frame(dhs_id = integer())
    wb_t4_df <- data.frame(dhs_id = integer())
  }
  
  if (nrow(ch_oda_sect_buff_sf) > 0) {  
    ch_t3_df <- st_join(dhs_buff_sf,
                        ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code <= 3, ],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      distinct(dhs_id)
    
    ch_t4_df <- st_join(dhs_buff_sf,
                        ch_oda_sect_buff_sf[ch_oda_sect_buff_sf$precision_code==4, ],
                        left=FALSE) %>%
      st_drop_geometry() %>%
      distinct(dhs_id)  
  } else {
    #create empty dataframes for use in rbind below
    ch_t3_df <- data.frame(dhs_id = integer())
    ch_t4_df <- data.frame(dhs_id = integer())
  }
  
  
  #identify control dhs_ids that don't intersect with either CH or WB projects
  control4_df <- dhs_buff_sf %>%
    st_drop_geometry() %>%
    distinct(dhs_id) %>%
    anti_join(rbind(
      wb_t4_df,
      wb_t3_df,
      ch_t4_df,
      ch_t3_df
    ) ,by="dhs_id")
  
  control3_df <- dhs_buff_sf %>%
    st_drop_geometry() %>%
    distinct(dhs_id) %>%
    anti_join(bind_rows(
      wb_t3_df,
      ch_t3_df
    ) ,by="dhs_id")

  #return sector-level descriptive statistics
  sector_stats <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(sector_stats) <- c('Sector','SectorName','WB Proj Locs',
                              'WB Treat<=p3','WB Treat<=p4','CH Proj Locs',
                              'CH Treat<=p3','CH Treat<=p4','DHS points',
                              'Controls<=p3','Controls<=p4')

  sector_stats[1,1] <- sector
  sector_stats[1,2] <- sector_name
  sector_stats[1,3] <- nrow(wb_oda_sect_buff_sf)
  sector_stats[1,4] <- nrow(wb_t3_df)
  sector_stats[1,5] <- nrow(wb_t4_df)
  sector_stats[1,6] <- nrow(ch_oda_sect_buff_sf)
  sector_stats[1,7] <- nrow(ch_t3_df)
  sector_stats[1,8] <- nrow(ch_t4_df)
  sector_stats[1,9] <- nrow(dhs_buff_sf)
  sector_stats[1,10] <- nrow(control3_df)
  sector_stats[1,11] <- nrow(control4_df)

  return(sector_stats)
  
}

###############################################################
## main code starts here
wb_oda_df <- read.csv("./data/interim/wb_africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,3,4)) #Exact, ADM2, ADM3
ch_oda_df <- read.csv("./data/interim/ch_africa_oda_sector_group.csv") %>% 
  filter(precision_code %in% c(1,3,4)) #Exact, ADM2, ADM3
africa_isos_df <- read.csv("./data/interim/africa_isos.csv")
debug_msg=as.logical(TRUE)

#generate vector and countries and sectors to pass to function
dhs_isos_v <- read.csv("./data/interim/dhs_clusters_id_est.csv") %>%
  select(iso3) %>%
  distinct()  %>%
  pull(iso3)

sectors_df <- unique(rbind(
            wb_oda_df %>% filter(site_iso3 %in% africa_isos_df$iso3) %>% distinct(ad_sector_codes, ad_sector_names),
            ch_oda_df %>% filter(site_iso3 %in% africa_isos_df$iso3) %>% distinct(ad_sector_codes, ad_sector_names))) %>%
            arrange(ad_sector_codes) 

projection <- "ESRI:102023"


####################################################
#### Load administrative borders
country_borders <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
  filter(ISO %in% africa_isos_df$iso3)
st_crs(country_borders) = "EPSG:4326"
country_borders <- st_transform(country_borders,crs=st_crs(projection))
country_borders <- sf::st_make_valid(country_borders)
sf::st_is_valid(country_borders)

adm1_borders <-  sf::read_sf("./data/country_regions/gadm1_clean.shp") %>%
  filter(ISO %in% africa_isos_df$iso3)
st_crs(adm1_borders) = "EPSG:4326"
adm1_borders <- st_transform(adm1_borders,crs=st_crs(projection))
adm1_borders <- sf::st_make_valid(adm1_borders)
sf::st_is_valid(adm1_borders)

adm2_borders <-  sf::read_sf("./data/country_regions/gadm2_clean.shp") %>%
  filter(ISO %in% africa_isos_df$iso3)
st_crs(adm2_borders) = "EPSG:4326"
adm2_borders <- st_transform(adm2_borders,crs=st_crs(projection))
adm2_borders <- sf::st_make_valid(adm2_borders)
sf::st_is_valid(adm2_borders)

#Load leader birthplaces for the country
plad_africa_adm1_sf <- geojsonsf::geojson_sf("./data/interim/plad_africa_adm1.geojson")
st_crs(plad_africa_adm1_sf) = "EPSG:4326"
plad_africa_adm1_sf <- st_transform(adm2_borders,crs=st_crs(projection))
plad_africa_adm1_sf <- sf::st_make_valid(adm2_borders)
sf::st_is_valid(plad_africa_adm1_sf)

####################################################
#### Process DHS survey points
dhs_sf <- read.csv("./data/interim/dhs_clusters_id_est.csv")  %>%
  group_by(iso3) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  st_as_sf(coords = c("lon", "lat"),crs="EPSG:4326") %>%
  select(dhs_id,year,rural,iwi,y_9,iso3,geometry)  %>%
  mutate(y_9 = y_9*100)


if (exists("dhs_sf", inherits=TRUE)) {
  dhs_sf <- st_transform(dhs_sf,crs=st_crs(projection))
  if (debug_msg) print(paste("Obs in dhs_sf:",nrow(dhs_sf)))
  
  #put a 2k or 5k buffer around DHS points, according to rural/urban privacy offset
  dhs_buff_sf <- rbind(
    st_buffer(dhs_sf[dhs_sf$rural==1,], 5000,  #meter units
              endCapStyle = "SQUARE"),
    st_buffer(dhs_sf[dhs_sf$rural==0,], 2000,  #meter units
              endCapStyle = "SQUARE")
  )
}

####################################################
sector_stats <- lapply(1:nrow(sectors_df), function(i) {
  sector <- sectors_df[i, "ad_sector_codes"]
  sector_name <- sectors_df[i, "ad_sector_names"]
  process_sectors(sector, sector_name, projection,
                  dhs_buff_sf, wb_oda_df,
                  ch_oda_df, country_borders, adm1_borders, adm2_borders,
                  plad_africa_adm1_sf, debug_msg)
})

sector_stats <- do.call(rbind,sector_stats) # add rows to dataframe
write.csv(sector_stats,"./data/interim/sector_treat_control_Africa.csv")

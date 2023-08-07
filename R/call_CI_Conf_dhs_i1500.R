# call_CI_Conf_dhs_i1500.R
# Desc:  Calls Causal Image Confounding over DHS points, using 5 satellite bands,
#        the same distribution of pre-treatment years for control and treated points,
#        by funder/sector combinations.  
library(causalimages)
library(dplyr)
library(tensorflow)

rm(list=ls())
setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")
args <- commandArgs(trailingOnly = TRUE)

# The first command line argument should be funder_sector (like both_110, wb_110, ch_110)
fund_sect_param <- args[1]
#uncomment to test
#fund_sect_param <- "both_140"
#fund_sect_param <- "wb_140"
#fund_sect_param <- "ch_140"

run <- "i1500"

dhs_df <-  read.csv("./data/interim/dhs_treat_control_confounders.csv") 

sector <- unique(sub(".*_(\\d+).*", "\\1", fund_sect_param))

if (substr(fund_sect_param,1,4)=="both") {
  #create the necessary columns for the "both" case, using wb columns for t/c
  # and getting min treatment year from both wb and ch
  dhs_df <- dhs_df %>% 
    filter(!!sym(paste0("wb_",sector)) %in% c(0,2)) %>% 
    mutate(!!sym(paste0("both_",sector)) := ifelse(get(paste0("wb_",sector)) == 2, 1, get(paste0("wb_",sector)))) %>% 
    rowwise() %>% mutate(
      !!sym(paste0("both_",sector,"_min_oda_year")) := min(!!sym(paste0("wb_",sector,"_min_oda_year")),
                                                           !!sym(paste0("ch_",sector,"_min_oda_year"))) 
    ) %>% ungroup() 
}

#define variable order and names for boxplots and dropped cols variables
var_order <- c("iwi_2017_2019_est","log_avg_nl_pre_oda","log_avg_pop_dens",
               "log_avg_min_to_city",
               "log_dist_km_to_gold","log_dist_km_to_gems",        
               "log_dist_km_to_dia","log_dist_km_to_petro", 
               "leader_birthplace","log_trans_proj_cum_n",
               "log_3yr_pre_conflict_deaths",
               "polity2","log_gdp_per_cap_USD2015","country_gini","landsat57",
               "landsat578")
var_labels <- c("Wealth 2017-2019 (est)","Nighlights (t-3,log)","Pop Density (t-1,log)",
                "Minutes to City (2000,log)","Dist to Gold (km,log)",
                "Dist to Gems (km,log)","Dist to Diam (km,log)",
                "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                "Conflict deaths (t-1,log)",
                "Country Polity2 (t-1)","Cntry GDP/cap (t-1,log)","Country gini (t-1)",
                "Landsat 5 & 7", "Landsat 5,7,& 8")

acquireImageRepFromDisk <- function(keys,training = F){
  imageHeight = 224  # 30m/pixel
  imageWidth = 224
  NBANDS = 5
  #will use bands BGR,NIR,SWIR1
  #specific layers will depend on year aid started; 
  #images have these years in these bands:
  # years     bands
  # 1996:1998 17:21
  # 1999:2001 25:29
  # 2002:2004 33:37
  # 2005:2007 41:45
  # 2008:2010 49:53
  # 2011:2013 57:61
  # 2014:2016 65:69
  
  #comment out to test
  # keys = paste0(sub_dhs_df$image_file,
  #               ifelse(is.na(sub_dhs_df[[paste0(fund_sect_param,"_min_oda_year")]]),"NA",
  #                      sub_dhs_df[[paste0(fund_sect_param,"_min_oda_year")]]))
  
  # initialize an array shell to hold image slices
  array_shell <- array(NA,dim = c(1L,imageHeight,imageWidth,NBANDS))

  # iterate over keys:
  # -- images are referenced to keys
  # -- keys are referenced to units (to allow for duplicate images uses)
  array_ <- sapply(keys,function(key_){
    #comment out to test 
    #key_ = "./data/dhs_tifs/angola_2006/00000.tifNA"
    #key_ = "./data/dhs_tifs/angola_2006/00000.tif2001"

    min_oda_year <- sub(".*\\.tif(.*)", "\\1", key_)
    image_file <- sub("(.*\\.tif).*", "\\1", key_)
        
    #min_oda_year <- 2002  #comment out to test
    #determine which year's images to get
    if (min_oda_year=="NA") {
      # don't expect NA's, but if happens, default to 1996:1998 (bands 17:21)
      bands_to_select <- c(17:21)
    } else {
      #select bands of the PREVIOUS (pre-project) period
      min_oda_year <- as.integer(min_oda_year)
      bands_to_select <- case_when(
                          min_oda_year %in% 1999:2001 ~ 17:21,
                          min_oda_year %in% 2002:2004 ~ 25:29,
                          min_oda_year %in% 2005:2007 ~ 33:37,
                          min_oda_year %in% 2008:2010 ~ 41:45,
                          min_oda_year %in% 2011:2013 ~ 49:53,
                          min_oda_year %in% 2014:2016 ~ 57:61)
    }

    bands_to_print <- paste(bands_to_select, collapse = " ")
    # used output message for testing, but produces large logfile
    # print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
    #       "read bands: ", bands_to_print,
    #       " year: ",min_oda_year,
    #       " from ",image_file))

    # iterate over all image bands
    for(i in 1:NBANDS) {
      band_ <- bands_to_select[i]
      im <- terra::rast(image_file,
                        lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                                    ,"_",band_))
      #rescale to original setting for RGB printing
      im <- im/.0001
      # place the image in the correct place in the array
      array_shell[,,,i] <- matrix(im, byrow = T, nrow = imageHeight, ncol = imageWidth)
    }
    return( array_shell )
  },
  simplify="array")  #using simplify = "array" combines images slices together

  # convert images to tensorflow array for further processing
  array_ <- tensorflow::tf$squeeze(tf$constant(array_,dtype=tf$float32),0L)
  array_ <- tensorflow::tf$transpose(array_,c(3L,0L,1L,2L))
  return( array_ )
}

  #subset the data for the current run
  sub_dhs_df <- dhs_df %>% 
    filter(!!sym(fund_sect_param) %in% c(0,1))
  treat_count <- sum(dhs_df[[fund_sect_param]] == 1)
  control_count <- sum(dhs_df[[fund_sect_param]] == 0)
  
  if (treat_count < 100) {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                " Skipping ",fund_sect_param," because fewer than 100 treated (",
                treat_count,")"))
    next 
  } else if (control_count == 0) {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                 " Skipping ",fund_sect_param," because no controls"))
    next 
  } else {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                 " Call AnalyzeImageConfounding for ",fund_sect_param,
                 ", treat n:",treat_count,
                 ", control n: ",control_count
                 ))
    
    #give control points the same distribution of years as treated points have
    #here, calculate the proportions and how many control points will get each year value
    year_props <- sub_dhs_df %>%
      filter(!!sym(fund_sect_param) == 1) %>% 
      group_by(!!sym(paste0(fund_sect_param, "_min_oda_year"))) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      mutate(proportion = count / treat_count) %>% 
      mutate(cntl_count = round(control_count * proportion)) %>% 
      rename(treat_year = !!(paste0(fund_sect_param, "_min_oda_year")))

    set.seed(1234)
    for (i in 1:nrow(year_props)) {
      #i=2  #uncomment to test
      
      # Randomly select control points to be assigned loop's current year
      dhs_ids_to_update <- sub_dhs_df %>%
        filter(!!sym(fund_sect_param) == 0) %>% 
        filter(is.na(!!sym(paste0(fund_sect_param, "_min_oda_year")))) %>% 
        slice_sample(n=year_props$cntl_count[i], replace = FALSE) %>% 
        pull(dhs_id)

      # Update randomly selected control dhs_ids with year of this loop
      sub_dhs_df <- sub_dhs_df %>%
        mutate(!!paste0(fund_sect_param, "_min_oda_year") := 
                 if_else(sub_dhs_df$dhs_id %in% dhs_ids_to_update,
                    year_props$treat_year[i],
                    !!sym(paste0(fund_sect_param, "_min_oda_year"))))
      
      #if we are in the last iteration, update remaining NA points (due to rounding
      # errors) to this year
      if (i==nrow(year_props)) {
        sub_dhs_df <- sub_dhs_df %>%
          mutate(!!paste0(fund_sect_param, "_min_oda_year") := 
                   if_else(is.na(!!sym(paste0(fund_sect_param, "_min_oda_year"))),
                           year_props$treat_year[i],
                           !!sym(paste0(fund_sect_param, "_min_oda_year"))))
      }
    }
    
    #adjust variables to be the year prior to each dhs point's first project year
    #in the sector. (or, for control points, the year assigned to match the
    #distribution of treatment first years)
    oda_year_column <- (paste0(fund_sect_param, "_min_oda_year"))
    
    sub_dhs_time_df <- sub_dhs_df %>% 
      mutate(
        log_avg_nl_pre_oda = case_when(
          !!sym(oda_year_column) %in% 2000:2001 ~ log_avg_nl_1996_1998,
          !!sym(oda_year_column) %in% 2002:2004 ~ log_avg_nl_1999_2001,
          !!sym(oda_year_column) %in% 2005:2007 ~ log_avg_nl_2002_2004,
          !!sym(oda_year_column) %in% 2008:2010 ~ log_avg_nl_2005_2007,
          !!sym(oda_year_column) %in% 2011:2013 ~ log_avg_nl_2008_2010,
          !!sym(oda_year_column) %in% 2014:2016 ~ log_avg_nl_2011_2013),
        log_dist_km_to_gold = case_when(
          !!sym(oda_year_column) %in% 2000:2001 ~ log_dist_km_to_gold_pre2001,
          !!sym(oda_year_column) > 2001 ~ log_dist_km_to_gold_2001),
        log_dist_km_to_petro = if_else(
          !!sym(oda_year_column) < 2003, 
          log_dist_km_to_petro_2000_2002,log_dist_km_to_petro_2003),
        #set dummy variables for the combination of satellite images in pre-project images
        #Landsat 5 only in images from 1990:1998 - won't include this column to avoid collinearity
        #Landsat 5&7 in images from    1999:2010 
        landsat57 = if_else(!!sym(oda_year_column) %in% 2002:2013,0,1),
        #Landsat 5,7, & 8 in images from 2011:2013
        landsat578 = if_else(!!sym(oda_year_column) %in% 2014:2016,0,1)
        #Landsat 7&8 in images from 2014:2019 - we aren't using any of these
        ) %>% 
      #set population density to year prior to earliest aid project
      rowwise() %>% mutate(
        log_avg_pop_dens = get(paste0("log_avg_pop_dens_", 
                                      as.numeric(get(oda_year_column)) - 1))
      ) %>% ungroup() %>% 
      #set leader_birthplace based on year prior to earliest aid project 
      rowwise() %>% mutate(
        leader_birthplace = get(paste0("leader_", as.numeric(get(oda_year_column)) - 1))
      ) %>% ungroup() %>% 
      #set conflict deaths to 3 year pre-project sum 
      rowwise() %>%  mutate(
        log_3yr_pre_conflict_deaths = get(paste0("log_deaths", 
                                                 as.numeric(get(oda_year_column)) - 3,
                                                 "_",
                                                 as.numeric(get(oda_year_column)) - 1))
      ) %>% ungroup() %>%  
      #set loan-based transport projects based on year prior to earliest aid project 
      rowwise() %>% mutate(
        log_trans_proj_cum_n = get(paste0("log_trans_proj_cum_n_",
                                          as.numeric(get(oda_year_column)) - 1))
      ) %>% ungroup()  

    #join to country-level parameters, which are year specific
    country_confounders_df <- read.csv("./data/interim/country_confounders.csv") %>% 
      select(-country) %>% 
      mutate(year=year+1)  #add 1 to year for join below, to get 1 year pre-project
    
    run_df <- sub_dhs_time_df %>% 
      left_join(country_confounders_df,
                by=join_by(iso3, !!sym(oda_year_column) == year))
    
    #write input data to file
    input_df <- run_df %>% 
      select(dhs_id, country, iso3, lat, lon, !!sym(fund_sect_param), 
             !!sym(oda_year_column), image_file, iwi_2017_2019_est,
             log_avg_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
             log_3yr_pre_conflict_deaths,log_trans_proj_cum_n,leader_birthplace,log_dist_km_to_gold,
             log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
             log_gdp_per_cap_USD2015,country_gini,polity2,landsat57,landsat578)  
    write.csv(input_df, paste0("./data/interim/input_",run,"_",fund_sect_param,".csv"),row.names = FALSE)
       
    if (nrow(input_df[!complete.cases(input_df),]) > 0) {
      print(paste0("Stopping because incomplete cases.  See ./data/interim/input_",
                   run,"_",fund_sect_param,".csv"))
    } else {
      conf_matrix <- as.matrix(data.frame(
        "log_avg_nl_pre_oda"         =input_df$log_avg_nl_pre_oda,          #scene level
        "log_avg_min_to_city"        =input_df$log_avg_min_to_city,         #scene level
        "log_avg_pop_dens"           =input_df$log_avg_pop_dens,            #scene level
        "log_3yr_pre_conflict_deaths"=input_df$log_3yr_pre_conflict_deaths, #inherited from ADM1
        "leader_birthplace"          =input_df$leader_birthplace,           #inherited from ADM1
        "log_trans_proj_cum_n"       =input_df$log_trans_proj_cum_n,        #inherited from ADM1, ADM2
        "log_dist_km_to_gold"        =input_df$log_dist_km_to_gold,         #scene level
        "log_dist_km_to_gems"        =input_df$log_dist_km_to_gems,         #scene level
        "log_dist_km_to_dia"         =input_df$log_dist_km_to_dia,          #scene level
        "log_dist_km_to_petro"       =input_df$log_dist_km_to_petro,        #scene level
        "log_gdp_per_cap_USD2015"    =input_df$log_gdp_per_cap_USD2015,     #country level
        "country_gini"               =input_df$country_gini,                #country level
        "polity2"                    =input_df$polity2,                     #country level
        "landsat57"                  =input_df$landsat57,                   #pre-treat image
        "landsat578"                 =input_df$landsat578                   #pre-treat image 
      ))
      #remove any columns that have 0 standard deviation before passing to function
      before_cols <-  colnames(conf_matrix)
      conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 
      dropped_cols <- paste(var_labels[match(setdiff(before_cols, colnames(conf_matrix)),var_order)],collapse="; ")
      if (dropped_cols != "") {
        print(paste("Dropped for 0 SD: ", dropped_cols))
      }
     
      ImageConfoundingAnalysis <- AnalyzeImageConfounding(
        obsW = input_df[[fund_sect_param]],
        obsY = input_df$iwi_2017_2019_est,  #lab's estimated iwi
        X = conf_matrix,
        long = input_df$lon,
        lat = input_df$lat,
        #concatenate the image file location and project year into a single keys parameter
        keys = paste0(input_df$image_file,
                      ifelse(is.na(input_df[[paste0(fund_sect_param,"_min_oda_year")]]),"NA",
                             input_df[[paste0(fund_sect_param,"_min_oda_year")]])), 
        acquireImageFxn = acquireImageRepFromDisk,
        samplingType = "balancedTrain",
        nSGD = 1500,
        nDepthHidden_conv = 5L, nDepthHidden_dense = 1L, maxPoolSize = 2L, strides = 2L, kernelSize = 3L,
        nFilters = 50L,
        figuresPath = "./figures/", # figures saved here
        plotBands=c(3,2,1),  #red, green, blue
        figuresTag = paste0(fund_sect_param,"_",run),
        conda_env = NULL, # conda env to try to activate
        conda_env_required = F
      )
  
      ica_df <- data.frame(t(unlist(ImageConfoundingAnalysis)))
      output_df <- cbind(data.frame(run,fund_sect_param,treat_count,control_count,
                                    dropped_cols,              #t_dropped_cols,c_dropped_cols,
                                    ica_df))
      print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                    " Writing to ./results/ICA_",fund_sect_param,"_",run,".csv"))
      write.csv(output_df,paste0("./results/ICA_",fund_sect_param,"_",run,".csv"),row.names = FALSE)

      ##########################################################################
      ##### generate boxplots for this run
      library(tidyr)     
      library(ggplot2)

      long_funder <- case_when(
        startsWith(fund_sect_param, "ch") ~ "China",
        startsWith(fund_sect_param, "wb") ~ "World Bank",
        startsWith(fund_sect_param, "both") ~ "Both China & World Bank"
      )
      
      sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
        mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")"))

      sector_name <- sector_names_df %>%
        filter(ad_sector_codes==sector) %>%
        pull(sec_pre_name)
      
      # Convert to long format for boxplots
      long_input_df <- input_df %>%
        select(-dhs_id, -country, -iso3, -lat, -lon, 
               -!!sym(oda_year_column), -image_file) %>% 
        gather(key=variable_name, value=value, -!!sym(fund_sect_param)) 
      

      sub_l1 <- paste("Funder:",long_funder,"     Sector:", sector_name)
      sub_l2 <- ifelse(nzchar(dropped_cols),
                       paste0("Dropped due to no variation: ", dropped_cols),
                       "")
      
                       # ifelse(nzchar(t_dropped_cols), paste0("; ", t_dropped_cols), ""),
                       # ifelse(nzchar(c_dropped_cols), paste0("; ", c_dropped_cols), ""))
      
      
      combined_boxplot <- ggplot(long_input_df, aes(x = factor(.data[[fund_sect_param]]), y = value)) +
        geom_boxplot() +
        labs(title = "Distribution of Wealth Outcome and Confounders for Treated and Control DHS locations",
             subtitle = paste(sub_l1,sub_l2,sep="\n"),
             x = paste0("Treatment/Control: 0:control (n ",control_count,"), 1:treated (n ",treat_count,")"),
             y = "Value") +
        facet_wrap(~ variable_name, scales = "free") +
        facet_wrap(~ factor(variable_name, levels = var_order, labels = var_labels), scales = "free") +
        theme_bw()
      
      
      ggsave(paste0("./figures/",fund_sect_param,"_",run,"_boxplots.png"),
             combined_boxplot,
             width=10, height = 8, dpi=300,
             bg="white", units="in")
      
      ##########################################################################
      ##### generate treatment/control map
      library(tmap)
      tmap_options(check.and.fix = TRUE)
      projection <- "ESRI:102023"
      
      #convert DHS points df to sf object
      input_sf <- sf::st_as_sf(input_df, coords=c("lon","lat"),crs="EPSG:4326")  %>%
        sf::st_transform(crs=sf::st_crs(projection))
      
      # Set the treated color based on funder
      treat_color <- case_when(
        startsWith(fund_sect_param, "ch") ~ "red",
        startsWith(fund_sect_param, "wb") ~ "blue",
        startsWith(fund_sect_param, "both") ~ "purple"
      )
      
      ####################################################
      #### Load administrative borders and ISO list excluding islands
      africa_map_isos_df <- read.csv("./data/interim/africa_map_isos.csv")
      
      country_borders <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
        filter(ISO %in% africa_map_isos_df$iso3)
      sf::st_crs(country_borders) = "EPSG:4326"
      country_borders <- sf::st_transform(country_borders,crs=sf::st_crs(projection))
      country_borders <- sf::st_make_valid(country_borders)
      
      ####################################################
      #### Generate map
      treat_control_map <- tm_shape(country_borders) +
        tm_borders(lwd=2) +
        tm_shape(input_sf[input_sf[[fund_sect_param]] == 0, ]) +  
        tm_dots(size=.3, col="gray", alpha=.3) +
        tm_shape(input_sf[input_sf[[fund_sect_param]] == 1, ]) +  
        tm_dots(size=.5, col=treat_color, alpha=.3) +
        tm_add_legend(type = "fill"
                      , col = c(treat_color,"gray")
                      , labels = c(paste0("Treated (n ",treat_count,")"),
                                   paste0("Control (n ",control_count,")")))  +
        tm_layout(main.title.size=1,
                  main.title = paste0(long_funder,": ",sector_name,"\nTreatment and Control Locations (2001-2014)"),
                  main.title.position=c("center","top"),
                  legend.position = c("left", "bottom"),
                  legend.text.size = 1,
                  frame = T ,
                  legend.outside = F, 
                  outer.margins = c(0, 0, 0, 0), 
                  legend.outside.size = .25
        )
      
      tmap_save(treat_control_map,paste0("./figures/",fund_sect_param,"_",run,"_map.png")) 
      
      #print these messages again to be at the end of the logfile
      if (dropped_cols != "") {
        print(paste("Dropped for 0 SD: ", dropped_cols))
      }
      # if (t_dropped_cols != "") {
      #   print(paste("Dropped for 0 SD in treatment group: ", t_dropped_cols))
      # }      
      # if (c_dropped_cols != "") {
      #   print(paste("Dropped for 0 SD in control group: ", c_dropped_cols))
      # } 
  }
}
         

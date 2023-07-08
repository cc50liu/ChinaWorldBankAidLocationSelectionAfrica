# call_CI_Conf_dhs_v6.R
# Desc:  Calls Causal Image Confounding over DHS points, using 5 satellite bands,
#        the same distribution of pre-treatment years for control and treated points,
#        by funder/sector combinations.  
#           v6 improves on v5 by:
#             - adjusting parameters to be based on project start year
# Use:  called from slurm script with command-line arg for funder_sector
#       examples:  sbatch cl_sector_v5_5b_2000i_year_dhs.slurm wb_140
#                  sbatch cl_sector_v5_5b_2000i_year_dhs.slurm ch_140
#                  sbatch cl_sector_v5_5b_2000i_year_dhs.slurm both_140
# Runs:  approximately 12 to 16 hours on SNIC/NAISS
library(causalimages)
library(dplyr)
library(tensorflow)

rm(list=ls())
setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")
args <- commandArgs(trailingOnly = TRUE)

# The first command line argument should be funder_sector (like both_110, wb_110, ch_110)
fund_sect_param <- args[1]
#comment out to test
#fund_sect_param <- "ch_140"
orig_fund_sect_param <- fund_sect_param  #for "both" case

run <- "v6"

dhs_df <-  read.csv("./data/interim/dhs_treat_control_confounders.csv") 

if (substr(fund_sect_param,1,4)=="both") {
  #get the both data out of the wb_sector column
  sector <- substr(fund_sect_param,5,nchar(fund_sect_param))
  fund_sect_param <- paste0("wb",sector)
  #adjust the data for the both case
  dhs_df <- dhs_df %>% 
    filter(!!sym(fund_sect_param) %in% c(0,2)) %>% 
    mutate(!!fund_sect_param := ifelse(get(fund_sect_param) == 2, 1, get(fund_sect_param)))
}

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
      # place the image in the correct place in the array
      array_shell[,,,i] <-
        as.matrix(terra::rast(image_file,
                              lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                                               ,"_",band_)))
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
                " Skipping ",orig_fund_sect_param," because fewer than 100 treated (",
                treat_count,")"))
    next 
  } else if (control_count == 0) {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                 " Skipping ",orig_fund_sect_param," because no controls"))
    next 
  } else {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                 " Call AnalyzeImageConfounding for ",orig_fund_sect_param,
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

    #produces this:
    # A tibble: 10 × 4
    # treat_year count proportion cntl_count
    # <int> <int>      <dbl>      <dbl>
    # 1       2000    10    0.0235         103
    # 2       2002     2    0.00469         21
    # 3       2003    32    0.0751         329
    # 4       2005     6    0.0141          62
    # 5       2006    41    0.0962         421
    # 6       2007    29    0.0681         298
    # 7       2008   125    0.293         1284
    # 8       2009    19    0.0446         195
    # 9       2012   112    0.263         1151
    # 10       2013    50    0.117          514
    
    
    #sum(year_props$cntl_count)
    #sum(year_props$proportion)

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
    
    #check results
    # sub_dhs_df %>%
    #   filter(!!sym(fund_sect_param) == 0) %>%
    #   group_by(!!sym(paste0(fund_sect_param, "_min_oda_year"))) %>%
    #   count()

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
        log_dist_km_to_petro = case_when(
          !!sym(oda_year_column)==2000 ~ log_dist_km_to_petro_1999,
          !!sym(oda_year_column) %in% 2001:2003 ~ log_dist_km_to_petro_2000_2002,
          !!sym(oda_year_column) > 2003 ~ log_dist_km_to_petro_2003)
        ) %>% 
      #set population density to year prior to earliest aid project
      #except for 2000, where we don't have 1999 density so use 2000
      rowwise() %>% mutate(
        log_avg_pop_dens = ifelse(get(oda_year_column)==2000, log_avg_pop_dens_2000,
                                   get(paste0("log_avg_pop_dens_", 
                                             as.numeric(get(oda_year_column)) - 1)))
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
      ) %>% ungroup()  
    
    #join to country-level parameters
    country_confounders_df <- read.csv("./data/interim/country_confounders.csv") %>% 
      select(-country)
    
    run_df <- sub_dhs_time_df %>% 
      left_join(country_confounders_df,
                by=join_by(iso3, !!sym(oda_year_column) == year))
    
    #write input data to file
    run_df %>% 
      select(dhs_id, country, iso3, lat, lon, !!sym(fund_sect_param), 
             !!sym(oda_year_column), image_file, iwi_2017_2019_est,
             log_avg_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
             log_3yr_pre_conflict_deaths,leader_birthplace,log_dist_km_to_gold,
             log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
             gdp_per_cap_USD2015,country_gini,polity2) %>% 
    write.csv(., paste0("./data/interim/input_",run,"_",orig_fund_sect_param,".csv"),row.names = FALSE)

    ImageConfoundingAnalysis <- AnalyzeImageConfounding(
      obsW = run_df[[fund_sect_param]],
      obsY = run_df$iwi_2017_2019_est,  #lab's estimated iwi
      X = scale(data.matrix(data.frame(
         "log_avg_nl_pre_oda"         =run_df$log_avg_nl_pre_oda,          #scene level
         "log_avg_min_to_city"        =run_df$log_avg_min_to_city,         #scene level
         "log_avg_pop_dens"           =run_df$log_avg_pop_dens,            #scene level
         "log_3yr_pre_conflict_deaths"=run_df$log_3yr_pre_conflict_deaths, #inherited from ADM1
         "leader_birthplace"          =run_df$leader_birthplace,           #inherited from ADM1
         "log_dist_km_to_gold"        =run_df$log_dist_km_to_gold,         #scene level
         "log_dist_km_to_gems"        =run_df$log_dist_km_to_gems,         #scene level
         "log_dist_km_to_dia"         =run_df$log_dist_km_to_dia,          #scene level
         "log_dist_km_to_petro"       =run_df$log_dist_km_to_petro,        #scene level
         "gdp_per_cap_USD2015"        =run_df$gdp_per_cap_USD2015,         #country level
         "country_gini"               =run_df$country_gini,                #country level
         "polity2"                    =run_df$polity2                      #country level
          )),center=TRUE, scale=TRUE),
      long = run_df$lon,
      lat = run_df$lat,
      #concatenate the image file location and project year into a single keys parameter
      keys = paste0(run_df$image_file,
                    ifelse(is.na(run_df[[paste0(fund_sect_param,"_min_oda_year")]]),"NA",
                                 run_df[[paste0(fund_sect_param,"_min_oda_year")]])), 
      acquireImageRepFxn = acquireImageRepFromDisk,
      samplingType = "balancedTrain",
      nSGD = 2000,
      nDepthHidden_conv = 5L, nDepthHidden_dense = 1L, maxPoolSize = 2L, strides = 2L, kernelSize = 3L,
      figuresPath = "./figures/", # figures saved here
      figuresTag = paste0(orig_fund_sect_param,"_",run),
      conda_env = NULL, # conda env to try to activate
      conda_env_required = F
    )

    ica_df <- data.frame(t(unlist(ImageConfoundingAnalysis)))
    output_df <- cbind(data.frame(run,orig_fund_sect_param,treat_count,control_count,ica_df))
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                  " Writing to ./data/interim/ICA_",run,"_",orig_fund_sect_param,".csv"))
    write.csv(output_df,paste0("./data/interim/ICA_",run,"_",orig_fund_sect_param,".csv"),row.names = FALSE)
  }

         

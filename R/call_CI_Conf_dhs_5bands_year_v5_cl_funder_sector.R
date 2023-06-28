# call_CI_Conf_dhs_5bands_year_v5_cl_funder_sector.R
# Desc:  Calls Causal Image Confounding over DHS points, using 5 satellite bands,
#        the same distribution of pre-treatment years for control and treated points,
#        by funder/sector combinations.  
#           v5 improves on v4 by:
#             - using the package version that includes uncertainty estimates
#             - reading a new consolidated confounders file that excludes problemmatic points
#             - adjusting nightlights to correspond with pre-project image year
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
#fund_sect_param <- "wb_140"
orig_fund_sect_param <- fund_sect_param  #for "both" case

run <- "v5_5b_2000i_dhs_year"

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

    #produces something like this:
    # treat_year count proportion cntl_count
    # <int> <int>      <dbl>      <dbl>
    # 1       2000   228     0.0475        208
    # 2       2001   434     0.0905        396
    # 3       2002   388     0.0809        354
    # 4       2003   829     0.173         757
    # 5       2004   519     0.108         474
    # 6       2005   371     0.0773        339
    # 7       2006   468     0.0975        428
    # 8       2007   223     0.0465        204
    # 9       2008   474     0.0988        433
    # 10       2009   120     0.0250        110
    # 11       2010   109     0.0227        100
    # 12       2011   173     0.0361        158
    # 13       2012   295     0.0615        269
    # 14       2013   116     0.0242        106
    # 15       2014    51     0.0106         47
    
    
    # sum(year_props$cntl_count)
    # sum(year_props$proportion)

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
    

    #create a nighlight variable for use in the analysis that corresponds to 
    #nightlights in the pre-project year
    oda_year_column <- (paste0(fund_sect_param, "_min_oda_year"))
    
    sub_dhs_df <- sub_dhs_df %>% 
      mutate(log_avg_nl_pre_oda = case_when(
        !!sym(oda_year_column) %in% 1999:2001 ~ log_avg_nl_1996_1998,
        !!sym(oda_year_column) %in% 2002:2004 ~ log_avg_nl_1999_2001,
        !!sym(oda_year_column) %in% 2005:2007 ~ log_avg_nl_2002_2004,
        !!sym(oda_year_column) %in% 2008:2010 ~ log_avg_nl_2005_2007,
        !!sym(oda_year_column) %in% 2011:2013 ~ log_avg_nl_2008_2010,
        !!sym(oda_year_column) %in% 2014:2016 ~ log_avg_nl_2011_2013))
    
    ImageConfoundingAnalysis <- AnalyzeImageConfounding(
      obsW = sub_dhs_df[[fund_sect_param]],
      obsY = sub_dhs_df$iwi_2017_2019_est,  #lab's estimated iwi
      X = scale(data.matrix(data.frame("log_avg_nl_pre_oda"=sub_dhs_df$log_avg_nl_pre_oda,  #scene level
                                 "log_avg_min_to_city"=sub_dhs_df$log_avg_min_to_city,    #scene level
                                 "log_avg_pop_dens_2000"=sub_dhs_df$log_avg_pop_dens_2000, #scene level
                                 "log_deaths1995_1999"=sub_dhs_df$log_deaths1995_1999,    #inherited from ADM1
                                 "leader_birthplace"=sub_dhs_df$leader_birthplace         #inherited from ADM1
                                 )),center=TRUE, scale=TRUE),
      long = sub_dhs_df$lon,
      lat = sub_dhs_df$lat,
      #concatenate the image file location and project year into a single keys parameter
      keys = paste0(sub_dhs_df$image_file,
                    ifelse(is.na(sub_dhs_df[[paste0(fund_sect_param,"_min_oda_year")]]),"NA",
                                 sub_dhs_df[[paste0(fund_sect_param,"_min_oda_year")]])), 
      acquireImageRepFxn = acquireImageRepFromDisk,
      nSGD = 2000,
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

         

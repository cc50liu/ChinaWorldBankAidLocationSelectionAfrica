#call_CausalImage_Confounding_dhs.R
library(causalimages)
library(dplyr)
library(tensorflow)

rm(list=ls())
setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")
args <- commandArgs(trailingOnly = TRUE)

# The first command line argument should be funder_sector (like both_110, wb_110, ch_110)
fund_sect_param <- args[1]
orig_fund_sect_param <- fund_sect_param  #useful in the both case

run <- "v3_5b_2000i_dhs_ADM2"

dhs_df <-  read.csv("./data/interim/dhs_treat_control_sector_vector.csv") 

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
  bands_to_select = c(17:22)
  NBANDS = length(bands_to_select)
  #the bands for 1996-1998 are 17:21 (BGR,NIR,SWIR1)
  #keys are the image_file location (e.g: "./data/dhs_tifs/chad_2014/00095.tif")

  # initialize an array shell to hold image slices
  array_shell <- array(NA,dim = c(1L,imageHeight,imageWidth,NBANDS))

  # iterate over keys:
  # -- images are referenced to keys
  # -- keys are referenced to units (to allow for duplicate images uses)
  array_ <- sapply(keys,function(key_){
    # iterate over all image bands
    #uncomment to test
    #key_ <- dhs_df[1,]$image_file
    for(i in 1:NBANDS) {
      band_ <- bands_to_select[i]
      # place the image in the correct place in the array
      array_shell[,,,i] <-
        as.matrix(terra::rast(key_,
                              lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=key_)
                                               ,"_",band_)))
    }
    return( array_shell )
  },
  simplify="array")  #using simplify = "array" combines images slices together

  # convert images to tensorflow array for further processing
  # note: your acquireImageRepFxn need not return tensorflow arrays.
  # R arrays are fine (with dimensions c(nBatch, imageWidth, imageHeight,nChannels)
  # (R arrays will be detected converted and converted internally)
  array_ <- tensorflow::tf$squeeze(tf$constant(array_,dtype=tf$float32),0L)
  array_ <- tensorflow::tf$transpose(array_,c(3L,0L,1L,2L))
  return( array_ )
}

  #subset the data for the current run
  sub_dhs_df <- dhs_df %>% 
    filter(!!sym(fund_sect_param) %in% c(0,1))
  treat_count <- sum(dhs_df[[fund_sect_param]] == 1)
  control_count <- sum(dhs_df[[fund_sect_param]] == 0)

  if (treat_count == 0) {
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                " Skipping ",orig_fund_sect_param," because no treated"))
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
    
    ImageConfoundingAnalysis <- AnalyzeImageConfounding(
      obsW = sub_dhs_df[[fund_sect_param]],
      obsY = sub_dhs_df$iwi_2017_2019_est,  #labs estimated iwi
      X = scale(data.matrix(data.frame("log_avg_nl_1996_1998"=sub_dhs_df$log_avg_nl_1996_1998,  #scene level
                                 "log_avg_min_to_city"=sub_dhs_df$log_avg_min_to_city,    #scene level
                                 "log_avg_pop_dens_2000"=sub_dhs_df$log_avg_pop_dens_2000, #scene level
                                 "log_deaths1995_1999"=sub_dhs_df$log_deaths1995_1999,    #inherited from ADM1
                                 "leader_birthplace"=sub_dhs_df$leader_birthplace         #inherited from ADM1
                                 )),center=TRUE, scale=TRUE),
      long = sub_dhs_df$lon,
      lat = sub_dhs_df$lat,
      keys = sub_dhs_df$image_file,
      acquireImageRepFxn = acquireImageRepFromDisk,
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

         

#!/usr/bin/env Rscript
#' Perform causal estimation under image confounding
#'
#' *Under beta release. Full release in Summer of 2023.*
#'
#' @usage
#'
#' AnalyzeImageConfounding(obsW, obsY, acquireImageFxn, ...)
#'
#' @param obsW A numeric vector where `0`'s correspond to control units and `1`'s to treated units.
#' @param obsY A numeric vector containing observed outcomes.
#' @param acquireImageRepFxn A function specifying how to load images representations associated with `imageKeysOfUnits` into memory. For example, if observation `3` has a value  of `"a34f"` in `imageKeysOfUnits`, `acquireImageFxn` should extract the image associated with the unique key `"a34f"`.
#' First argument should be image key values and second argument have be `training` (in case behavior in training/)
#' @param acquireImageFxn (default = `acquireImageRepFxn`) Similar to `acquireImageRepFxn`; this is a function specifying how to load images associated with `imageKeysOfUnits` into memory.
#' @param transportabilityMat (optional) A matrix with a column named `keys` specifying keys to be used by `acquireImageRepFxn` for generating treatment effect predictions for out-of-sample points.
#' @param imageKeysOfUnits (default = `1:length(obsY)`) A vector of length `length(obsY)` specifying the unique image ID associated with each unit. Samples of `imageKeysOfUnits` are fed into `acquireImageFxn` to call images into memory.
#' @param long,lat (optional) Vectors specifying longitude and latitude coordinates for units. Used only for describing highest and lowest probability neighorhood units if specified.
#' @param X (optional) A numeric matrix containing tabular information used if `orthogonalize = T`. `X` is normalized internally and salience maps with respect to `X` are transformed back to the original scale.
#' @param conda_env (default = `NULL`) A string specifying a conda environment wherein `tensorflow`, `tensorflow_probability`, and `gc` are installed.
#' @param conda_env_required (default = `F`) A Boolean stating whether use of the specified conda environment is required.
#' @param figuresTag (default = `""`) A string specifying an identifier that is appended to all figure names.
#' @param figuresPath (default = `"./"`) A string specifying file path for saved figures made in the analysis.
#' @param plotBands (default = `1L`) An integer or vector specifying which band position (from the acquired image representation) should be plotted in the visual results. If a vector, `plotBands` should have 3 (and only 3) dimensions (corresponding to the 3 dimensions to be used in RBG plotting).
#' @param kernelSize (default = `5L`) Dimensions used in convolution kernels.
#' @param nSGD (default = `400L`) Number of stochastic gradient descent (SGD) iterations.
#' @param nBoot (default = `100L`) Number of bootstrap iterations for uncertainty estimation.
#' @param typeBoot (default = `SamplingOnly`) Bootstrap type. `typeBoot = 'SamplingOnly'` captures sampling uncertainty only. `typeBoot = 'EstimationAndSampling'` captures both estimation and sampling uncertainty.
#' @param batchSize (default = `50L`) Batch size used in SGD optimization.
#' @param doConvLowerDimProj (default = `T`) Should we project the `nFilters` convolutional feature dimensions down to `nDimLowerDimConv` to reduce the number of required parameters.
#' @param nDimLowerDimConv (default = `3L`) If `doConvLowerDimProj = T`, then, in each convolutional layer, we project the `nFilters` feature dimensions down to `nDimLowerDimConv` to reduce the number of parameters needed.
#' @param nFilters (default = `50L`) Integer specifying the number of convolutional filters used.
#' @param nDenseWidth (default = `32L`) Width of dense projection layers post-convolutions.
#' @param nDepthHidden_conv (default = `3L`) Hidden depth of convolutional layer.
#' @param nDepthHidden_dense (default = `0L`) Hidden depth of dense layers. Default of `0L` means a single projection layer is performed after the convolutional layer (i.e., no hidden layers are used).
#' @param quiet (default = `F`) Should we suppress information about progress?
#' @param maxPoolSize (default = `2L`) Integer specifying the max pooling size used in the convolutional layers.
#' @param strides (default = `2L`) Integer specifying the strides used in the convolutional layers.=
#' @param simMode (default = `F`) Should the analysis be performed in comparison with ground truth from simulation?
#' @param tf_seed (default = `NULL`) Specification for the tensorflow seed.
#' @param plotResults (default = `T`) Should analysis results be plotted?
#' @param channelNormalize (default = `T`) Should channelwise image feature normalization be attempted? Default is `T`, as this improves training.
#'
#' @return A list consiting of \itemize{
#'   \item `ATE_est` ATE estimate.
#'   \item `ATE_se` Standard error estimate for the ATE.
#'   \item `(images saved to disk if plotResults = T)` If `plotResults = T`, causal salience plots are saved to disk characterizing the image confounding structure. See references for details.
#' }
#'
#' @section References:
#' \itemize{
#' \item  Connor T. Jerzak, Fredrik Johansson, Adel Daoud. Integrating Earth Observation Data into Causal Inference: Challenges and Opportunities. *ArXiv Preprint*, 2023.
#' }
#'
#' @examples
#' # For a tutorial, see
#' # github.com/cjerzak/causalimages-software/
#'
#' @export
#' @md

#######################################
#    Cindy code
##########################################    
rm(list=ls())
library(dplyr)
library(tensorflow)
fund_sect_param = "ch_140"
sector <- unique(sub(".*_(\\d+).*", "\\1", fund_sect_param))
oda_year_column <- (paste0(fund_sect_param, "_min_oda_year"))
input_df <- read.csv("./data/interim/input_v16r_ch_140.csv")
run <- "v17"

dhs_df <-  read.csv("./data/interim/dhs_treat_control_confounders.csv") 

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

keys="./data/dhs_tifs/south_africa_2016/00738.tif2008"

dhs_df %>%
  filter((round(lat,3)==-26.910 & round(lon,3)==26.595) |
         (round(lat,3)==-27.402 & round(lon,3)==26.648)  |
         (round(lat,3)==-26.281 & round(lon,3)==27.888) |
         (round(lat,3)==-8.758 & round(lon,3)==13.500) |
         (round(lat,3)==-11.492 & round(lon,3)==43.386) |
         (round(lat,3)==-11.438 & round(lon,3)==43.392) |
           (round(lat,3)==-34.463 & round(lon,3)==19.542) |
           (round(lat,3)==-34.419 & round(lon,3)==19.189) |
           (round(lat,3)==-20.213 & round(lon,3)==28.520) |
           (round(lat,3)==-20.208 & round(lon,3)==28.555)) %>%
  select(dhs_id, country, image_file, lat, lon)


# dhs_id      country                                  image_file        lat      lon
# 1   48830 south_africa ./data/dhs_tifs/south_africa_2016/00743.tif -34.463232 19.54247
# 2   48781 south_africa ./data/dhs_tifs/south_africa_2016/00694.tif -34.418873 19.18893
# 3   48237 south_africa ./data/dhs_tifs/south_africa_2016/00150.tif -27.402460 26.64820
# 4   48607 south_africa ./data/dhs_tifs/south_africa_2016/00520.tif -26.910471 26.59503
# 5   48316 south_africa ./data/dhs_tifs/south_africa_2016/00229.tif -26.281482 27.88798
# 6   55822     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00028.tif -20.213279 28.52031
# 7   55800     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00006.tif -20.207657 28.55544
# 8    8158      comoros      ./data/dhs_tifs/comoros_2012/00149.tif -11.491992 43.38610
# 9    8161      comoros      ./data/dhs_tifs/comoros_2012/00152.tif -11.437967 43.39200
# 10     44       angola       ./data/dhs_tifs/angola_2006/00044.tif  -8.757972 13.50009



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

      keys <- c("./data/dhs_tifs/angola_2006/00044.tif2013",
                "./data/dhs_tifs/comoros_2012/00149.tif2013",
                "./data/dhs_tifs/comoros_2012/00152.tif2013",
                "./data/dhs_tifs/south_africa_2016/00150.tif2013",
                "./data/dhs_tifs/south_africa_2016/00229.tif2013",
                "./data/dhs_tifs/south_africa_2016/00520.tif2013")
      
      for (key_ in keys) {
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
        
        #for 2010, the max is messed up for Comoros
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2010 41 max(im_matrix) is  0.191449999809265"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2010 42 max(im_matrix) is  0.247299998998642"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2010 43 max(im_matrix) is  0.305849999189377"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2010 44 max(im_matrix) is  0.438300013542175"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2010 45 max(im_matrix) is  0.519999980926514"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2010 41 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2010 42 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2010 43 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2010 44 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2010 45 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2010 41 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2010 42 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2010 43 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2010 44 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2010 45 max(im_matrix) is  2"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2010 41 max(im_matrix) is  0.26230001449585"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2010 42 max(im_matrix) is  0.327950000762939"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2010 43 max(im_matrix) is  0.344749987125397"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2010 44 max(im_matrix) is  0.420800000429153"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2010 45 max(im_matrix) is  0.51529997587204"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2010 41 max(im_matrix) is  0.36175000667572"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2010 42 max(im_matrix) is  0.461549997329712"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2010 43 max(im_matrix) is  0.499300003051758"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2010 44 max(im_matrix) is  0.49099999666214"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2010 45 max(im_matrix) is  0.517350018024445"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2010 41 max(im_matrix) is  0.387650012969971"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2010 42 max(im_matrix) is  0.496950000524521"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2010 43 max(im_matrix) is  0.546649992465973"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2010 44 max(im_matrix) is  0.562600016593933"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2010 45 max(im_matrix) is  0.556249976158142"
        
        #for 2013 south africa 520 is over 1
        #"./data/dhs_tifs/south_africa_2016/00520.tif2013 51 max(im_matrix) is  1.425950050354"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2013 49 max(im_matrix) is  0.206249997019768"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2013 50 max(im_matrix) is  0.281650006771088"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2013 51 max(im_matrix) is  0.317999988794327"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2013 52 max(im_matrix) is  0.459699988365173"
        # [1] "./data/dhs_tifs/angola_2006/00044.tif2013 53 max(im_matrix) is  0.537500023841858"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2013 49 max(im_matrix) is  0.214499995112419"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2013 50 max(im_matrix) is  0.309899985790253"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2013 51 max(im_matrix) is  0.364300012588501"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2013 52 max(im_matrix) is  0.621999979019165"
        # [1] "./data/dhs_tifs/comoros_2012/00149.tif2013 53 max(im_matrix) is  0.386599987745285"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2013 49 max(im_matrix) is  0.214499995112419"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2013 50 max(im_matrix) is  0.309899985790253"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2013 51 max(im_matrix) is  0.364300012588501"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2013 52 max(im_matrix) is  0.586700022220612"
        # [1] "./data/dhs_tifs/comoros_2012/00152.tif2013 53 max(im_matrix) is  0.386599987745285"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2013 49 max(im_matrix) is  0.240600004792213"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2013 50 max(im_matrix) is  0.283800005912781"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2013 51 max(im_matrix) is  0.307799994945526"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2013 52 max(im_matrix) is  0.418500006198883"
        # [1] "./data/dhs_tifs/south_africa_2016/00150.tif2013 53 max(im_matrix) is  0.503400027751923"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2013 49 max(im_matrix) is  0.38850000500679"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2013 50 max(im_matrix) is  0.490900009870529"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2013 51 max(im_matrix) is  0.557200014591217"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2013 52 max(im_matrix) is  0.510699987411499"
        # [1] "./data/dhs_tifs/south_africa_2016/00229.tif2013 53 max(im_matrix) is  0.533550024032593"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2013 49 max(im_matrix) is  0.370550006628036"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2013 50 max(im_matrix) is  0.476850003004074"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2013 51 max(im_matrix) is  1.425950050354"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2013 52 max(im_matrix) is  0.528249979019165"
        # [1] "./data/dhs_tifs/south_africa_2016/00520.tif2013 53 max(im_matrix) is  0.566100001335144"
        
        bands_to_print <- paste(bands_to_select, collapse = " ")
        # used output message for testing, but produces large logfile
        # print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
        #       "read bands: ", bands_to_print,
        #       " year: ",min_oda_year,
        #       " from ",image_file))
        
        # iterate over all image bands
        for(i in 1:5) {
          band_ <- bands_to_select[i]
          im <- terra::rast(image_file,
                            lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                                        ,"_",band_))
          
          im_matrix <- matrix(im, byrow = T, nrow = 244, ncol = 244)
          
          print(paste(key_,band_,"max(im_matrix) is ",max(im_matrix)))
      }
      }
      
      
      # iterate over keys:
      # -- images are referenced to keys
      # -- keys are referenced to units (to allow for duplicate images uses)
      array_ <- sapply(keys,function(key_){
        #comment out to test 
        #key_ = "./data/dhs_tifs/angola_2006/00000.tifNA"
        #key_ = "./data/dhs_tifs/angola_2006/00000.tif2001"
        #key_ = "./data/dhs_tifs/south_africa_2016/00738.tif2008"  #beach
        #key_ = "./data/dhs_tifs/comoros_2012/00149.tif2010"  #comoros island
        #key_ = "./data/dhs_tifs/south_africa_2016/00229.tif2002"  #sa medium light
        #key_ = "./data/dhs_tifs/angola_2006/00044.tif2001" #angola - reasonable color at 1000 scale
        
        # dhs_id      country                                  image_file        lat      lon
        # 1     44       angola       ./data/dhs_tifs/angola_2006/00044.tif  -8.757972 13.50009
        # 2   8158      comoros      ./data/dhs_tifs/comoros_2012/00149.tif -11.491992 43.38610
        # 3   8161      comoros      ./data/dhs_tifs/comoros_2012/00152.tif -11.437967 43.39200
        # 4  48237 south_africa ./data/dhs_tifs/south_africa_2016/00150.tif -27.402460 26.64820
        # 5  48316 south_africa ./data/dhs_tifs/south_africa_2016/00229.tif -26.281482 27.88798
        # 6  48607 south_africa ./data/dhs_tifs/south_africa_2016/00520.tif -26.910471 26.59503
        
        

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
        i=3
        for(i in 1:NBANDS) {
          band_ <- bands_to_select[i]
          im <- terra::rast(image_file,
                            lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                                        ,"_",band_))
         
          terra::plot(im) 
          terra::plotRGB(im,r=3, g=2, b=1)
          dev.off()
          
          min_val <- min(im)
          max_val <- max(im)
          
          # Rescale the vector to the [0, 255] range
          im_rescaled <- ((im - min_val) / (max_val - min_val)) * 255
          
          terra::plot(im_rescaled) 
          
          #rescale for RGB printing
          im_rescaled_800 <- im * 800
          im_rescaled <- im/.0001
          terra::plot(im_rescaled) 
          max(im_rescaled)
          max(im)
          
          max(im_rescaled_800)
          terra::plot(im_rescaled_850) 
          im_rescaled_1000 <- im * 1000
          terra::plot(im_rescaled_1000) 
          im_rescaled_10000 <- im * 10000
          terra::plot(im_rescaled_10000) 
          dev.off()
          terra::plotRGB(im_rescaled_1000,r=3, g=2, b=1)
          
          dev.off()
          terra::plotRGB(im_rescaled_10000,r=3, g=2, b=1)

          
          array_shell[,,,i] <- matrix(im_rescaled, byrow = T, nrow = imageHeight, ncol = imageWidth)
          
          
          terra::plot(im_rescaled)
          dev.off()

          # place the image in the correct place in the array
          #connor's:
          
          #failed:
          #original:
          #array_shell[,,,i] <- as.matrix(im)
          #array_shell[,,,i] <- terra::as.matrix(im,wide=FALSE)
          #array_shell[,,,i] <- terra::as.matrix(im,wide=TRUE) produced error
          #array_shell[,,,i] <- base::as.matrix(im)
          #array_shell[,,,i] <- base::as.matrix(im)
        }
        return( array_shell )
      },
      simplify="array")  #using simplify = "array" combines images slices together
      
      # convert images to tensorflow array for further processing
      
      # note: your acquireImageRepFxn need not return tensorflow arrays. 
      # R arrays are fine (with dimensions c(nBatch, imageWidth, imageHeight,nChannels)
      # (R arrays will be detected converted and converted internally)
      #dim(array_)
      #1 224 224   5   1
      
      array_ <- tensorflow::tf$squeeze(tf$constant(array_,dtype=tf$float32),0L)
      #dim(array_)
      #224 224   5   1
      array_ <- tensorflow::tf$transpose(array_,c(3L,0L,1L,2L))
      #dim(array_)
      #1 224 224   5
      
      
      # dev.off()
      # image2(
      #   as.matrix( array_[,,1] ),
      #   main = long_lat_in_, cex.main = 2.5, col.main =  col_
      # ) 
      # 
      # dev.off()
      # causalimages::image2(
      #   terra::as.matrix( array_[,,,plotBands[1]] ),
      #   main = long_lat_in_, cex.main = 2.5, col.main =  col_
      # ) 
      # 
      
      return( array_ )
    }
    
zzz    
   #raster library
   #lat/lon  -11.491992 43.38610
   comoros_r <- raster::brick("./data/dhs_tifs/comoros_2012/00149.tif")
   comoros_rgb <-  comoros_r[[c(1,2,3)]]
   comoros_scaled <- comoros_rgb/.0001
   raster::plotRGB(comoros_scaled,r=3,g=2,b=1)
   dev.off()
   raster::plotRGB(comoros_scaled,r=3,g=2,b=1,stretch="lin")
   dev.off()
   raster::plotRGB(comoros_scaled,r=3,g=2,b=1,stretch="hist")
   
   #lat/lon -34.463232 19.54247
   safrica_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00743.tif")
   safrica_rgb <-  safrica_r[[c(1,2,3)]]
   safrica_scaled <- safrica_rgb/.0001
   raster::plotRGB(safrica_scaled,r=3,g=2,b=1)
   dev.off()
   raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="lin")
   dev.off()
   raster::plotRGB(safrica_scaled,r=3,g=2,b=1,stretch="hist")
   
   #lat/lon -34.418873 19.18893
   safrica2_r <- raster::brick("./data/dhs_tifs/south_africa_2016/00694.tif")
   safrica2_rgb <-  safrica2_r[[c(1,2,3)]]
   safrica2_scaled <- safrica2_rgb/.0001
   raster::plotRGB(safrica2_scaled,r=3,g=2,b=1)
   dev.off()
   raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="lin")
   dev.off()
   raster::plotRGB(safrica2_scaled,r=3,g=2,b=1,stretch="hist")
   
   
   # dhs_id      country                                  image_file        lat      lon
   # 1   48830 south_africa ./data/dhs_tifs/south_africa_2016/00743.tif -34.463232 19.54247
   # 
   # 2   48781 south_africa ./data/dhs_tifs/south_africa_2016/00694.tif -34.418873 19.18893
   # 3   48237 south_africa ./data/dhs_tifs/south_africa_2016/00150.tif -27.402460 26.64820
   # 4   48607 south_africa ./data/dhs_tifs/south_africa_2016/00520.tif -26.910471 26.59503
   # 5   48316 south_africa ./data/dhs_tifs/south_africa_2016/00229.tif -26.281482 27.88798
   # 6   55822     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00028.tif -20.213279 28.52031
   # 7   55800     zimbabwe     ./data/dhs_tifs/zimbabwe_1999/00006.tif -20.207657 28.55544
   # 8    8158      comoros      ./data/dhs_tifs/comoros_2012/00149.tif -11.491992 43.38610
   # 9    8161      comoros      ./data/dhs_tifs/comoros_2012/00152.tif -11.437967 43.39200
   # 10     44       angola       ./data/dhs_tifs/angola_2006/00044.tif  -8.757972 13.50009
   
   
   
   
    #terra library
    # comoros <- terra::rast("./data/dhs_tifs/comoros_2012/00149.tif",
    #                   lyrs=c("00149_41","00149_42","00149_43"))
    # 
    # #black
    # terra::plotRGB(comoros,r=3, g=2, b=1)
    # terra::plotRGB(comoros,r="00149_43", g="00149_42", b="00149_41")
    # 
    # #looks like I saw in the output
    # comoros_scaled <-  terra::stretch(comoros,minv=0,maxv=255)
    # 
    # #try manually clamping and adjusting for the gamma value
    # comoros_clamped <- terra::clamp(comoros,lower=0,upper=3000,values=TRUE)
    # comoros_gamma_corrected <- comoros_clamped ^ (1/1.4)  #1.4 is the gamma value in GEE documentation
    # 
    # max(comoros_gamma_corrected)
    # terra::plotRGB(comoros_gamma_corrected,r=3, g=2, b=1)
    
    dev.off()
    
    
    
#default values from function parameters    
    X = NULL
    file = NULL
    keys = NULL
    nDepth = 3L
    doConvLowerDimProj = T
    nDimLowerDimConv = 3L
    nFilters = 50L
    samplingType = "none"
    doHiddenDim = T
    nBoot = 100L
    typeBoot = "SamplingOnly"
    HiddenDim  = 32L
    DenseActivation = "linear"
    input_ave_pooling_size = 1L # if seeking to downshift the resolution
    useTrainingPertubations = T
    orthogonalize = F
    imageKeysOfUnits = 1:length(input_df)
    acquireImageRepFxn = NULL
    acquireImageFxn = NULL
    transportabilityMat = NULL
    lat = NULL
    long = NULL
    conda_env = NULL
    conda_env_required = F
    figuresTag = ""
    figuresPath = "./"
    plotBands = 1L
    simMode = F
    plotResults = T
    nDepthHidden_conv = 1L
    nDepthHidden_dense = 0L
    maxPoolSize = 2L
    strides = 1L
    compile = T
    batchSize = 50L
    kernelSize = 3L
    nSGD  = 400L
    nDenseWidth = 32L
    channelNormalize = T
    printDiagnostics = F
    tf_seed = NULL
    quiet = F
    
    ##### from Cindy calling code
    
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
      before_cols <-  colnames(conf_matrix)
      conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 
      dropped_cols <- paste(var_labels[match(setdiff(before_cols, colnames(conf_matrix)),var_order)],collapse="; ")
      if (dropped_cols != "") {
        print(paste("Dropped for 0 SD: ", dropped_cols))
      }
      
     #set parameters to values of caller
      obsW = input_df[[fund_sect_param]]
      obsY = input_df$iwi_2017_2019_est  #lab's estimated iwi
      X = conf_matrix
      long = input_df$lon
      lat = input_df$lat
      #concatenate the image file location and project year into a single keys parameter
      keys = paste0(input_df$image_file,
                    ifelse(is.na(input_df[[paste0(fund_sect_param,"_min_oda_year")]]),"NA",
                           input_df[[paste0(fund_sect_param,"_min_oda_year")]])) 
      acquireImageRepFxn = acquireImageRepFromDisk
      samplingType = "balancedTrain"
      nSGD = 2
      nDepthHidden_conv = 5L
      nDepthHidden_dense = 1L
      maxPoolSize = 2L
      strides = 2L
      kernelSize = 3L
      nFilters = 50L
      figuresPath = "./figures/" # figures saved here
      plotBands=c(3,2,1)  #red, green, blue
      figuresTag = paste0(fund_sect_param,"_",run)
      conda_env = NULL # conda env to try to activate
      conda_env_required = F
      
#########################################      
      
    
    library(tensorflow); library(keras)
    try(tensorflow::use_condaenv(conda_env, required = conda_env_required),T)
    Sys.sleep(1.); try(tf$square(1.),T); Sys.sleep(1.)
    try(tf$config$experimental$set_memory_growth(tf$config$list_physical_devices('GPU')[[1]],T),T)
    try( tf$config$set_soft_device_placement( T ) , T)
    #tfd <- (tfp <- tf_probability())$distributions
    #tfa <- reticulate::import("tensorflow_addons")

    try(tf$random$set_seed(  c( ifelse(is.null(tf_seed),
                                yes = 123431L, no = as.integer(tf_seed)  ) )), T)
    try(tf$keras$utils$set_random_seed( c( ifelse(is.null(tf_seed),
                                yes = 123419L, no = as.integer(tf_seed)  ) )), T)

    # import python garbage collectors
    py_gc <- reticulate::import("gc")
    gc(); py_gc$collect()


  if(!is.null(X)){ if(!"matrix" %in% class(X)){
    print("Coercing X to matrix class...")
    X <- data.matrix( X )
  } }

  if(!is.null(X)){ if(is.na(sum(X))){
    stop("Error: is.na(sum(X)) is TRUE; check for NAs or that all variables are numeric.")
  }}

  if(!is.null(X)){ if(any(apply(X,2,sd) == 0)){
    stop("Error: any(apply(X,2,sd) == 0) is TRUE; a column in X seems to have no variance; drop column!")
  }}

  #if(!is.null(X)){ if( abs(mean(apply(X,2,sd))-1)>0.01 | abs(mean(apply(X,2,mean))-0)>0.01){print("Note: We noticed that X is not normalized. Normalizing X (to mean 0, sd = 1) is recommended!...") }}
  if(!is.null(X)){
    X_mean <- colMeans(X)
    X_sd <- apply(X,2,sd)
    X <- t( (t(X) - X_mean ) / (0.00001+X_sd) )
  }

  {
    acquireImageMethod <- "functional";
    # define base tf record + train/test fxns
    if(  !is.null(  file  )  ){
      acquireImageMethod <- "tf_record"

      # established tfrecord connection
      orig_wd <- getwd()
      tf_record_name <- file
      tf_record_name <- strsplit(tf_record_name,split="/")[[1]]
      new_wd <- paste(tf_record_name[-length(tf_record_name)],collapse = "/")
      setwd( new_wd )
      tf_dataset = tf$data$TFRecordDataset(  tf_record_name[length(tf_record_name)] )

      # helper functions
      getParsed_tf_dataset_inference <- function(tf_dataset){
        dataset <- tf_dataset$map( parse_tfr_element ) # return
        return( dataset <- dataset$batch( as.integer(max(2L,round(batchSize/2L)  ))) )
      }

      getParsed_tf_dataset_train <- function(tf_dataset){
        dataset <- tf_dataset$map( parse_tfr_element )
        dataset <- dataset$shuffle(tf$constant(as.integer(10*batchSize),dtype=tf$int64),
                                   reshuffle_each_iteration = T)
        dataset <- dataset$batch(as.integer(batchSize))
      }

      # setup iterators
      tf_dataset_train <- getParsed_tf_dataset_train( tf_dataset )
      tf_dataset_inference <- getParsed_tf_dataset_inference( tf_dataset )

      # reset iterators
      ds_iterator_train <- reticulate::as_iterator( tf_dataset_train )
      ds_iterator_inference <- reticulate::as_iterator( tf_dataset_inference )

      # checks
      # ds_iterator_inference$output_shapes; ds_iterator_train$output_shapes
      # ds_next_train <- reticulate::iter_next( ds_iterator_train )
      # ds_next_inference <- reticulate::iter_next( ds_iterator_inference )
      setwd(  orig_wd  )
    }

    trainingPertubations <- tf$identity
    if(useTrainingPertubations){
      trainingPertubations <- (function(im__){
        #with( tf$device('/CPU:0'), {
          im__ <- tf$image$random_flip_left_right(im__)
          im__ <- tf$image$random_flip_up_down(im__)
          return( im__ )
        #})
    })
    }

    binaryCrossLoss <- function(W,prW){return( - mean( log(prW+0.001)*W + log(1-prW+0.001)*(1-W) ) ) }

    InitImageProcess <- function(im, training = F, input_ave_pooling_size = 1){
      print("start InitImageProcess")
      # expand dims if needed
      if(length(keys) == 1){ im <- tf$expand_dims(im,0L) }

      # normalize
      im <- (im - NORM_MEAN_array) / NORM_SD_array

      # training pertubations if desired
      # note: trainingPertubations mus be performed on CPU
      if(training == T){ 
        im <- trainingPertubations(im) 
        print("InitImageProcess called trainingPertubations")
      } else
        print("InitImageProcess did NOT call trainingPertubations")
      {
        
      }

      # downshift resolution if desired
      if(input_ave_pooling_size > 1){ im <- AvePoolingDownshift(im) }
      return( im  )
    }

    # some hyperparameters parameters
    figuresPath <- paste(strsplit(figuresPath,split="/")[[1]],collapse = "/")
    KernalActivation <- "swish"
    KernalProjActivation <- "swish"
    HiddenActivation <- "swish"
    BN_MOMENTUM <- 0.90
    poolingAt <- 1L # do pooling every poolingAt iterations
    poolingBy <- 2L # pool by poolingBy by poolingBy
    poolingType <- "max"
    LEARNING_RATE_BASE <- 0.005; widthCycle <- 50
    doParallel <- F
    testIndices <- trainIndices <- 1:length(obsY)

    # initialize layers
    AvePoolingDownshift <- tf$keras$layers$AveragePooling2D(pool_size = as.integer(c(input_ave_pooling_size,input_ave_pooling_size)))
    try(eval(parse(text = paste("rm(", paste(trainable_layers,collapse=","),")"))),T)
    trainable_layers <- ls()
    {
      GlobalMaxPoolLayer <- tf$keras$layers$GlobalMaxPool2D(data_format="channels_last",name="GlobalMax")
      GlobalAvePoolLayer <- tf$keras$layers$GlobalAveragePooling2D(data_format="channels_last",name="GlobalAve")
      GlobalPoolLayer <- function(z){
        return(tf$concat(list(GlobalMaxPoolLayer(z),GlobalAvePoolLayer(z)),1L)) }
      BNLayer_Axis1_inputDense <- tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T, momentum = BN_MOMENTUM, epsilon = 0.001)
      BNLayer_Axis1_hidden <- tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T, momentum = BN_MOMENTUM, epsilon = 0.001)
      #BNLayer_Axis1_final <- tf$keras$layers$BatchNormalization(axis = 1L, center = T, scale = T, momentum = BN_MOMENTUM, epsilon = 0.001)
      HiddenProjection <- tf$keras$layers$Dense(HiddenDim, activation = "linear")
      DenseLayer <- tf$keras$layers$Dense(1L, activation = DenseActivation)
      FlattenLayer <- tf$keras$layers$Flatten(data_format = "channels_last")

      # define tf function
      tf_function_use  <- tf_function
      #tf_function_use  <- function(.){.}

      #getMeanConv <- tf_function_use( function(dar){ 1./kernelSize^2*tf$squeeze(Conv_Mean( tf$expand_dims(dar,3L) ),3L)} )
      BNLayer_Axis3_init <- tf$keras$layers$BatchNormalization(axis = 3L, center = F, scale = F, momentum = BN_MOMENTUM, epsilon = 0.001,name="InitNorm")
      for(d_ in 1:nDepth){
        eval(parse(text = sprintf('Conv%s <- tf$keras$layers$Conv2D(filters=nFilters,
                                  kernel_size=c(kernelSize,kernelSize),
                                  activation=KernalActivation,
                                  strides = c(strides,strides),
                                  padding = "valid")',d_)))
        eval(parse(text = sprintf('ConvProj%s = tf$keras$layers$Dense(nDimLowerDimConv,
                                      activation=KernalProjActivation)',d_)))
        eval(parse(text = sprintf('BNLayer_Axis3_%s <- tf$keras$layers$BatchNormalization(axis = 3L, center = T, scale = T, momentum = BN_MOMENTUM, epsilon = 0.001)',d_)))
        eval(parse(text = sprintf('BNLayer_Axis3_%s_inner <- tf$keras$layers$BatchNormalization(axis = 3L, center = T, scale = T, momentum = BN_MOMENTUM, epsilon = 0.001)',d_)))

        # every third, do pooling
        if(d_ %% poolingAt == 0){
          Pool = tf$keras$layers$MaxPool2D(pool_size = c(poolingBy,poolingBy))
          if(poolingType=="ave"){ Pool = tf$keras$layers$AveragePooling2D(pool_size = c(poolingBy,poolingBy)) }
        }
        if(d_ %% poolingAt != 0){ eval(parse(text = sprintf('Pool%s = tf$identity',d_))) }
      }
    }
    trainable_layers <- ls()[!ls() %in% c(trainable_layers)]
    getProcessedImage <- tf_function_use( function(imm,training){
      # convolution + pooling
      for(d_ in 1:nDepth){
        if(doConvLowerDimProj){
          eval(parse(text = sprintf("imm <- BNLayer_Axis3_%s_inner(Pool( Conv%s( imm )),training=training)",d_,d_)))
          if(d_ < nDepth){ eval(parse(text = sprintf("imm <- BNLayer_Axis3_%s(  ConvProj%s( imm ), training = training)",d_,d_))) }
        }
        if(doConvLowerDimProj == F){ eval(parse(text = sprintf("imm <- BNLayer_Axis3_%s( Pool( Conv%s( imm )), training = training)",d_,d_,d_)))}
      }
      return(imm)
    })

    getTreatProb <- tf_function_use( function(im_getProb,x_getProb, training_getProb){

      # flatten
      im_getProb <- GlobalPoolLayer( getProcessedImage(im_getProb,training = training_getProb) )
      im_getProb <- BNLayer_Axis1_inputDense(im_getProb, training = training_getProb)

      # concatinate with scene-level data
      im_getProb <- tf$concat(list(im_getProb,x_getProb),1L)

      # optimal hidden layer
      if(doHiddenDim == T){
        im_getProb <-  tf$keras$activations$swish( HiddenProjection(  im_getProb   ) )
        im_getProb <- BNLayer_Axis1_hidden( im_getProb )
      }

      # final projection layer + sigmoid
      im_getProb <- DenseLayer( im_getProb   )
      im_getProb <- tf$keras$activations$sigmoid( im_getProb )

      # return
      return( im_getProb )
    })
    epsilonLabelSmooth <- tf$constant(0.01)
    getLoss <- tf_function_use( function(im_getLoss, x_getLoss, treatt_getLoss, training_getLoss){
      treatProb <- getTreatProb( im_getProb = im_getLoss,
                                 x_getProb = x_getLoss,
                                 training_getProb = training_getLoss )
      treatt_r <- tf$cast(tf$reshape(treatt_getLoss,list(-1L,1L)),dtype=tf$float32)
      treatProb_r <- tf$reshape(treatProb,list(-1L,1L)) # check

      # final loss
      minThis <- tf$negative( tf$reduce_mean( tf$multiply(tf$math$log( tf$maximum(treatProb_r,0.001)),  (treatt_r)) +
                                      tf$multiply(tf$math$log(tf$maximum(1-treatProb_r,0.001)),  (1-treatt_r)) ))
      return( minThis )
    })

    # get first iter batch for initializations
    print("Calibrating first moments for input data normalization...")
    NORM_SD <- NORM_MEAN <- c()
    for(momentCalIter in 1:(momentCalIters<-10)){
      if(acquireImageMethod == "tf_record"){
        ds_next_train <- reticulate::iter_next( ds_iterator_train )
        batch_indices <- as.array(ds_next_train[[2]])
      }
      if(acquireImageMethod == "functional"){
        batch_indices <- sample(1:length(obsY),batchSize,replace = F)
        ds_next_train <- list(
          r2const( acquireImageRepFxn(keys[batch_indices],) , dtype = tf$float32 )
        )
      }

      # setup normalizations
      if(is.null(NORM_MEAN)){
        NORM_MEAN <- NORM_SD <- apply(as.array(ds_next_train[[1]]),4,sd)
        NORM_MEAN[] <- NORM_SD[] <- 0
      }

      # update normalizations
      NORM_SD <- NORM_SD + apply(as.array(ds_next_train[[1]]),4,sd) / momentCalIters
      NORM_MEAN <- NORM_MEAN + apply(as.array(ds_next_train[[1]]),4,mean) / momentCalIters
    }
    NORM_MEAN_array <- tf$constant(array(NORM_MEAN,dim=c(1,1,1,length(NORM_MEAN))),tf$float32)
    NORM_SD_array <- tf$constant(array(NORM_SD,dim=c(1,1,1,length(NORM_SD))),tf$float32)

    # arms
    print("Initializing traiing arms...")
    # new TF version kills compiled fxn here (use 2.12)
    for(ARM in c(T,F)){
      with(tf$GradientTape() %as% tape, {
        myLoss_forGrad <- getLoss( im_getLoss = InitImageProcess(im = ds_next_train[[1]],
                                                                 training = T,
                                                                 input_ave_pooling_size = input_ave_pooling_size),
                                   x_getLoss = tf$constant(X[batch_indices,],tf$float32),
                                   treatt_getLoss = tf$constant(as.matrix(obsW[batch_indices]),tf$float32 ),
                                   training_getLoss = ARM )
      })
    }
    trainable_variables <- tape$watched_variables()

    # initialize beta
    init_beta_ref <- c(sapply(init_beta <- seq(-4,4,length.out = 1000),function(zer){ mean( 1/(1+exp(- (rnorm(1000) + zer))) )} ))
    init_beta <- init_beta [ which.min(abs(init_beta_ref - mean(obsW) ) ) ]
    if(samplingType == "initializeBeta"){
      print("INITIALIZING beta");BNLayer_Axis1_final$trainable_variables[[2]]$assign( tf$expand_dims(tf$constant(init_beta,dtype=tf$float32),0L) ) # beta is offset factor
    }

    # define optimizer and training step
    NA20 <- function(zer){zer[is.na(zer)] <- 0;zer[is.infinite(zer)] <- 0;zer}
    optimizer_tf = tf$optimizers$legacy$Nadam()
    getGrad <- tf_function_use(function(im_train, x_train, truth_train){
      with(tf$GradientTape() %as% tape, {
        myLoss_forGrad <- getLoss( im_getLoss = im_train,
                                   x_getLoss = x_train,
                                   treatt_getLoss = truth_train,
                                   training_getLoss = T)
      })
      my_grads <- tape$gradient( myLoss_forGrad, trainable_variables )
      return(list(myLoss_forGrad,my_grads))
    })
    trainStep <- (function(im_train, x_train, truth_train){
      my_grads <- getGrad(im_train, x_train, truth_train)
      myLoss_forGrad <- my_grads[[1]]
      my_grads <- my_grads[[2]]
      optimizer_tf$learning_rate$assign(   tf$constant(LEARNING_RATE_BASE*abs(cos(i/nSGD*widthCycle))*(i<nSGD/2)+
                                                         NA20(LEARNING_RATE_BASE*(i>=nSGD/2)/(i-nSGD/2+1)^.3) ) )
      optimizer_tf$apply_gradients( rzip(my_grads, trainable_variables)[!unlist(lapply(my_grads,is.null)) ])
      return(  list(myLoss_forGrad, my_grads)  )
    })

    # number of trainable variables
    nTrainable <- sum( unlist(  lapply(trainable_variables,function(zer){ prod(dim(zer)) }) ) )
    print(sprintf("%s Trainable Parameters",nTrainable))


    if(typeBoot == "EstimationAndSampling"){
        stop("Option `typeBoot='EstimationAndSampling'` under construction")
    }

    # perform training
    print("Starting training sequence...")
    loss_vec <- rep(NA,times=nSGD)
    in_ <- ip_ <- 0; for(i in 1:nSGD){
      if((i %% 100 == 0 | (i == 10) | i == nSGD) & doParallel == F | i < 50){
        print(sprintf("Iteration: %i",i) );
        try(par(mfrow = c(1,1)),T);try(plot(loss_vec),T); try(points(smooth.spline(na.omit(loss_vec)),type="l",lwd=3),T)
      }
      if(i %% 10 == 0){ py_gc$collect() }
      if((i %% 10 == 0 | i == 1 ) & doParallel == T){
        write.csv(file = sprintf("./checkpoint%s.csv",CommandArg_i), data.frame("CommandArg_i"=CommandArg_i, "i"=i))
      }

      if(i == 1){ batch_indices_past <- myLoss_forGrad_past <- NA }
      if(i > 1){ myLoss_forGrad_past <- myLoss_forGrad; batch_indices_past <- batch_indices }

      if(acquireImageMethod == "functional"){
        if(samplingType != "balancedTrain"){
          batch_indices <- sample(trainIndices,batchSize,replace=F)
        }
        if(samplingType == "balancedTrain"){
          batch_indices <- c(sample(trainIndices[which(obsW[trainIndices]==1)], batchSize/2),
                             sample(trainIndices[which(obsW[trainIndices]==0)], batchSize/2) )
        }
        ds_next_train <- list(
          r2const( acquireImageRepFxn(keys[batch_indices],) , dtype = tf$float32 )
        )
      }

      if(acquireImageMethod == "tf_record"){
        ds_next_train <- reticulate::iter_next( ds_iterator_train )

        # if we run out of observations, reset iterator...
        if(is.null(ds_next_train)){
          tf$random$set_seed(as.integer(runif(1,1,1000000)))
          tf_dataset_train <- tf_dataset_train$`repeat`()
          ds_iterator_train <- reticulate::as_iterator( tf_dataset_train )
          #ds_next_train <- reticulate::iter_next( ds_iterator_train )
        }

        # if we haven't run out of observations, set up data
        if(!is.null(ds_next_train)){
          if(length(as.array(ds_next_train[[2]])) < batchSize){
            tf_dataset_train <- getParsed_tf_dataset_train( tf_dataset )
            ds_iterator_train <- reticulate::as_iterator( tf_dataset_train )
          }
        }
        batch_indices <- c(as.array(ds_next_train[[2]]))
      }

      myLoss_forGrad <- trainStep(
        im_train = InitImageProcess(ds_next_train[[1]],
                                    training = T,
                                    input_ave_pooling_size = input_ave_pooling_size),
        x_train = tf$constant(X[batch_indices,],dtype=tf$float32),
        truth_train = tf$constant(as.matrix(obsW[batch_indices]),tf$float32))

      # post-processing checks
      loss_vec[i] <- as.numeric( myLoss_forGrad[[1]] )
      grad_norm <- f2n(try(sum(unlist(lapply(myLoss_forGrad[[2]],function(zer){sum(as.numeric(zer)^2)}))),T))
      if(is.na(loss_vec[i] ) | is.na(grad_norm) ){
        print("NA in loss -- opening browser")
        print("Image sum:")
        print(as.numeric(tf$math$reduce_sum( InitImageProcess(ds_next_train[[1]],
              training = T, input_ave_pooling_size = input_ave_pooling_size) )))
        print("Prior recent losses:")
        try( print(loss_vec[(i-10):i]), T)
        print("Keys:")
        print( ds_next_train[[1]] )
        print("Batch indices:")
        print( batch_indices )
        print("Table of batch sampled W's:" )
        print(table(  obsW[batch_indices] ))
        print("Sum of batch sampled X's:" )
        print(sum(  X[batch_indices,] ))
        browser()
        stop("NA introduced in training! Check images + input data for NAs. Try increasing batch size.")
      }
    }
    print("Done with training sequence...")

    # remove big objects to free memory for inference
    rm(ds_next_train);rm(myLoss_forGrad)

    # get probabilities for inference
    print("Starting to get probabilities for inference...")
    gc();py_gc$collect()
    prWEst_convnet <- rep(NA,times = length(obsW))
    last_i <- 0; ok_counter <- 0; ok<-F;while(!ok){
      ok_counter <- ok_counter + 1
      print(sprintf("[%s] %.2f%% done with getting inference probabilities", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 100*last_i / length(obsW)))

      # in functional mode
      if(acquireImageMethod == "functional"){
        batch_indices_inference <- (last_i+1):(last_i+batchSize)
        batch_indices_inference <- batch_indices_inference[batch_indices_inference<=length(obsW)]
        last_i <- batch_indices_inference[length(batch_indices_inference)]
        if(last_i == length(obsW)){ ok <- T }

        batchSizeOneCorrection <- F; if(length(batch_indices_inference) == 1){
          batch_indices_inference <- c(batch_indices_inference,batch_indices_inference)
          batchSizeOneCorrection <- T
        }

        batch_inference <- list(
          r2const( acquireImageRepFxn(keys[batch_indices_inference],) , dtype = tf$float32 )
        )

        insert_probs <- try(c(as.array(getTreatProb(im_getProb = InitImageProcess(batch_inference[[1]],
                                                               input_ave_pooling_size = input_ave_pooling_size),
                                                    x_getProb = tf$constant(X[batch_indices_inference,],dtype=tf$float32),
                                                    training_getProb = F ))),T)
        if( "try-error" %in% class(insert_probs)){
          print("Error in generating insert_probs in line 491! Investigating...")
          print("Output:")
          print( insert_probs )
          print("batch_inference[[1]]:")
          print(batch_inference[[1]])
          stop("Stopping due to try-error in insert_probs generation...")
        }
        if(batchSizeOneCorrection){ insert_probs <- insert_probs[-1]; batch_indices_inference <- batch_indices_inference[-1] }
        prWEst_convnet[batch_indices_inference] <- insert_probs
      }

      # in tf record mode
      if(acquireImageMethod == "tf_record"){
        batch_inference <- reticulate::iter_next( ds_iterator_inference )
        ok<-T;if(!all(is.null(batch_inference))){
          ok<-F
          batch_indices_inference <- as.array(batch_inference[[2]])
          drop_<-F;if(length(batch_indices_inference)==1){
            drop_ <- T
            batch_indices_inference<-c(batch_indices_inference,batch_indices_inference)
            batch_inference[[1]] <- tf$concat(list(batch_inference[[1]],batch_inference[[1]]),0L)
          }
          insert_probs <- try(c(as.array(getTreatProb(im_getProb = InitImageProcess(batch_inference[[1]],
                                                                                    input_ave_pooling_size = input_ave_pooling_size),
                                                      x_getProb = tf$constant(X[batch_indices_inference,],dtype=tf$float32),
                                                      training_getProb = F ))),T)
          if(drop_ == T){  insert_probs <- insert_probs[-1]  }
          if("try-error"  %in% class(insert_probs)){browser()}
          prWEst_convnet[batch_indices_inference] <- insert_probs
        }
      }

      gc();py_gc$collect()
    }
    rm( batch_inference )

    # clip extreme estimated probabilities
    prWEst_convnet[prWEst_convnet<0.01] <- 0.01
    prWEst_convnet[prWEst_convnet>0.99] <- 0.99
    print(   cor( c(obsW),c(prWEst_convnet) ) )
    if(any(is.na(prWEst_convnet)) ){
      print("Error: NAs in estimated probabilities! Reporting debugging information now...")
      print("Printing summary of probabilities...")
      print(summary( prWEst_convnet ) )
      print("Printing first probabilities...")
      print(head( prWEst_convnet ))
      print("Printing last probabilities...")
      print(tail( prWEst_convnet ))
      print("Printing NA indices...")
      print(which(is.na( prWEst_convnet  )))
      stop("Shutting down now due to NAs (see prior debugging messages)...")
    }

    # compute base loss
    prWEst_base <- prWEst_convnet
    prWEst_base[] <- mean(obsW[-testIndices])
    baseLoss_ce_ <- binaryCrossLoss(obsW[testIndices], prWEst_base[testIndices])
    baseLossIN_ce_ <- binaryCrossLoss(obsW[trainIndices], prWEst_base[trainIndices])
    baseLoss_class_ <- 1/length(testIndices) * (sum( prWEst_base[testIndices][ obsW[testIndices] == 1] < 0.5) +
                                                  sum( prWEst_base[testIndices][ obsW[testIndices] == 0] > 0.5))

    outLoss_class_ <- 1/length(testIndices) * (sum( prWEst_convnet[testIndices][ obsW[testIndices] == 1] < 0.5) +
                                                 sum( prWEst_convnet[testIndices][ obsW[testIndices] == 0] > 0.5))
    outLoss_ce_ <-  binaryCrossLoss(  obsW[testIndices], prWEst_convnet[testIndices]  )
    inLoss_ce_ <-  binaryCrossLoss(  obsW[trainIndices], prWEst_convnet[trainIndices]  )

    # do some analysis with examples
    processedDims <- NULL
    if(    plotResults == T  ){
      print("Starting to plot the image confounding results...")
      # get treatment image
      testIndices_t <- testIndices[which(obsW[testIndices]==1)]
      testIndices_c <- testIndices[which(obsW[testIndices]==0)]

      showPerGroup <- min(c(3,unlist(table(obsW))), na.rm = T)
      top_treated <- testIndices_t[indices_top_t <- order( prWEst_convnet[testIndices_t] ,decreasing=T)[1:(showPerGroup*3)]]
      #2705 1735 1692 1672 1673 1533 1758 3839 1537
      top_control <- testIndices_c[indices_top_c <- order( prWEst_convnet[testIndices_c] ,decreasing=F)[1:(showPerGroup*3)]]
      #622  623  709  662  708  666  630 1221  629
      # drop duplicates
      longLat_test_t <- paste(round(long[testIndices_t],1L),
                              round(lat[testIndices_t],1L),sep="_")
      longLat_test_c <- paste(round(long[testIndices_c],1L),
                              round(lat[testIndices_c],1L),sep="_")
      top_treated <- top_treated[!duplicated(longLat_test_t[indices_top_t])][1:showPerGroup]
      #2705 1735 1692
      top_control <- top_control[!duplicated(longLat_test_c[indices_top_c])][1:showPerGroup]
      #622 623 709
      plot_indices <- c(top_control, top_treated)
      #622  623  709 2705 1735 1692
      
      #find the image with the obvious beach
      grep("./data/dhs_tifs/south_africa_2016/00738.tif",keys)
      #16
      keys[16]
      #"./data/dhs_tifs/south_africa_2016/00738.tif2008"
      
      makePlots <- function(){

        try({
        pdf(sprintf("%s/CSM_KW%s_AvePool%s_Tag%s.pdf",
                    figuresPath,
                    kernelSize,
                    input_ave_pooling_size,
                    figuresTag),
            width = length(plot_indices)*5+2,height = 3*5)
        {
          dev.off()
          layout(matrix(1:(3*(1+length(plot_indices))),
                        ncol = 1+length(plot_indices)),
                 width = c(0.5,rep(5,length(plot_indices))),
                 height = c(5,5,5)); in_counter <- 0
          for(text_ in c("Raw Image","Salience Map","Final Spatial Layer")){
            par(mar=c(0,0,0,0))
            plot(0, main = "", ylab = "",cex=0,
                 xlab = "", ylim = c(0,1), xlim = c(0,1),
                 xaxt = "n",yaxt = "n",bty = "n")
            text(0.5,0.5,labels = text_, srt=90,cex=3)
          }


          #read original file and plot it with terra's plot function
          image_file <- "./data/dhs_tifs/south_africa_2016/00738.tif"
          band_=33
          lyrs_=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                       ,"_",band_)
          im <- terra::rast(image_file,
                            lyrs=lyrs_)
          #rescale for RGB printing
          im <- im * 1000
          
          terra::plot(im)
          #horizontal beach          
          
          
          #read original file and plot it with raster's plot function
          im_r <- raster::raster(image_file,
                            bands=lyrs_)
          #rescale for RGB printing
          im_r <- im_r * 1000
          
          raster::plot(im_r)
          #horizontal beach  
          
          for(in_ in plot_indices){
            in_ <- 16

            if(acquireImageMethod == "tf_record"){
              ds_next_in <- GetElementFromTfRecordAtIndex( index = in_,
                                                           filename = file )
              if(length(ds_next_in$shape) == 3){ ds_next_in[[1]] <- tf$expand_dims(ds_next_in[[1]], 0L) }
            }
            zzz
            if(acquireImageMethod == "functional"){
              ds_next_in <- r2const( acquireImageRepFxn(keys[],), dtype = tf$float32 )
              
             if(length(ds_next_in$shape) == 3){ ds_next_in <- tf$expand_dims(ds_next_in,0L) }
              ds_next_in <- list( ds_next_in )
            }

            #byrow = T: beach is vertical immediately after reading in acquireImageRepFxn and plotting with my copy of image2, but upside down
            #byrow = F: vertical


            
            #byrow = T: beach is horizontal immediately after reading in acquireImageRepFxn and plotting with my copy of image2, but upside down
            #byrow = F: horizontal and upside down
            im_test <- as.array(tf$squeeze(ds_next_in[[1]],c(0L)))
            
            dev.off()
            causalimages::image2(
              terra::as.matrix( im_test[,,plotBands[1]] ),
              cex.main = 2.5, 
            ) 
            
            
            dev.off()
            #my custom version of the function
            image2(
              terra::as.matrix( im_test[,,plotBands[1]] ),
              cex.main = 2.5
            ) 
            
                        
            print(in_)
            col_ <- ifelse(in_ %in% top_treated,
                           yes = "black", no = "gray")
            in_counter <- in_counter + 1
            long_lat_in_ <- sprintf("Lat, Long: %.3f, %.3f",
                                    lat[in_],long[in_])
            #"Lat, Long: -34.054, 18.641"
            # extract
            im_orig <- im_ <- InitImageProcess(
                                im = ds_next_in[[1]],
                                training = F,
                                input_ave_pooling_size = input_ave_pooling_size)
            
            #at this point, the beach is vertical - rotated and flipped
            im_ <- as.array(tf$squeeze(im_,c(0L)))
            dev.off()
            causalimages::image2(
              as.matrix( im_[,,plotBands[1]] ),
              main = long_lat_in_, cex.main = 2.5, col.main =  col_
            )            
            
            XToConcat_values <- tf$constant(t(X[in_,]),tf$float32)
            im_processed <- getProcessedImage(im_, training = F)
            processedDims <- dim(im_)
            im_ <- as.array(tf$squeeze(im_,c(0L)))
            
            #at this point, the beach is vertical - rotated and flipped
            dev.off()
            causalimages::image2(
              as.matrix( im_[,,plotBands[1]] ),
              main = long_lat_in_, cex.main = 2.5, col.main =  col_
            )

            # calculate salience map
            im_orig <- tf$Variable(im_orig,trainable = T)
            with(tf$GradientTape() %as% tape, {
              tape$watch(im_orig)
              treat_prob_im <- tf$squeeze(tf$squeeze(getTreatProb( im_getProb = im_orig,
                                                                   x_getProb = XToConcat_values,
                                                                   training_getProb = F),0L),0L)
            })

            salience_map <- tape$gradient( treat_prob_im, im_orig )
            salience_map <- tf$math$reduce_euclidean_norm(salience_map,3L,keepdims=T)
            salience_map <- tf$keras$layers$AveragePooling2D(c(3L,3L))(salience_map)
            salience_map <- as.array(salience_map)[1,,,]
            salience_map <- apply(salience_map^2,1:2,sum)^0.5

            # do plotting
            orig_scale_im_ <- sapply(1:length(NORM_MEAN),
                                     function(band_){
                                       im_[,,band_] <- 0.1+im_[,,band_]*NORM_SD[band_] + NORM_MEAN[band_]
                                       im_[,,band_] }, simplify="array")
            par(mar = (mar_vec <- c(2,1,3,1)))

            # plot raw image
            if(length(plotBands) < 3){
              causalimages::image2(
                as.matrix( orig_scale_im_[,,plotBands[1]] ),
                main = long_lat_in_, cex.main = 2.5, col.main =  col_
              )
            }
            if(length(plotBands) >= 3){
               plot(0, main = long_lat_in_,col.main = col_,
                    ylab = "", xlab = "", cex.main = 4, ylim = c(0,1), xlim = c(0,1),
                    cex = 0, xaxt = "n",yaxt = "n",bty = "n")
               orig_scale_im_raster <- raster::brick(orig_scale_im_[,,plotBands[1:3]])
               raster::plotRGB(orig_scale_im_raster, r=1, g=2, b=3, add = T, main = long_lat_in_)
            }

            # plot salience map
            par(mar = mar_vec)
            salience_map[salience_map>0] <- salience_map[salience_map>0] / (0.001+sd(salience_map[salience_map>0]))
            print(summary(c(salience_map)))
            salience_map <- sign(salience_map)*log(abs(salience_map)+1)
            print(summary(c(salience_map)))
            causalimages::image2( salience_map )

            # plot final layer
            par(mar = mar_vec)
            causalimages::image2( as.array(im_processed)[1,,,1] )
          }
        }
        dev.off()
        }, T)

        try({
        pdf(sprintf("%s/Hist_KW%s_AvePool%s_Tag%s.pdf",
                    figuresPath,
                    kernelSize,
                    input_ave_pooling_size,
                    figuresTag))
        {
          par(mfrow=c(1,1))
          d0 <- density(prWEst_convnet[obsW==0])
          d1 <- density(prWEst_convnet[obsW==1])
          plot(d1,lwd=2,xlim = c(0,1),ylim =c(0,max(c(d1$y,d0$y),na.rm=T)*1.2),
               cex.axis = 1.2,ylab = "",xlab = "",
               main = "Density Plots for \n Estimated Pr(T=1 | Confounders)",cex.main = 2)
          points(d0,lwd=2,type = "l",col="gray",lty=2)
          text(d0$x[which.max(d0$y)[1]],
               max(d0$y,na.rm=T)*1.1,label = "T = 0",col="gray",cex=2)
          text(d1$x[which.max(d1$y)[1]],
               max(d1$y,na.rm=T)*1.1,label = "T = 1",col="black",cex=2)
        }
        dev.off()
        }, T)
      }

      if(plotResults){  try(makePlots(),T) }

      # compute salience for tabular covariates
      SalienceX <- NULL; if(!is.null(X)){
        getSalienceVec <- function(im_, x_){
          x_ <- tf$Variable(x_,trainable = T)
          with(tf$GradientTape() %as% tape, {
            tape$watch(x_)
            treat_prob_im <- tf$squeeze(tf$squeeze(getTreatProb( im_getProb = im_,
                                                                 x_getProb = x_,
                                                                 training_getProb = F),0L),0L)
          })
          return(  salience_vec <- tape$gradient( treat_prob_im, x_ )   ) }
        SalienceX <- c(); samp_counter <- 0
        for(samp_ in sample(1:nrow(X),100,replace = T)){
          print(sprintf("Tabular Salience Iteration %s of %s", samp_counter <- samp_counter + 1, 100))
          if(acquireImageMethod == "tf_record"){
            ds_next_in <- GetElementFromTfRecordAtIndex( index = samp_,
                                                         filename = file )
            if(length(ds_next_in$shape) == 3){ ds_next_in[[1]] <- tf$expand_dims(ds_next_in[[1]], 0L) }
          }
          if(acquireImageMethod == "functional"){
            ds_next_in <- r2const( acquireImageRepFxn(keys[ samp_ ],), dtype = tf$float32 )
            if(length(ds_next_in$shape) == 3){ ds_next_in <- tf$expand_dims(ds_next_in,0L) }
            ds_next_in <- list( ds_next_in )
          }

        im_ <- InitImageProcess(
                          ds_next_in[[1]],
                          input_ave_pooling_size = input_ave_pooling_size)
        x_ <- tf$constant(t(X[samp_,]),tf$float32)
        SalienceX <- rbind(SalienceX,as.matrix( getSalienceVec(im_=im_, x_=x_)))
        }
        SalienceX <- colMeans( SalienceX ); names( SalienceX ) <- colnames(X)

        # rescale the salience map into original scale
        SalienceX <- SalienceX*X_sd  +   X_mean
      }

      preDiff <- colMeans(cbind(long[obsW == 1],lat[obsW == 1])) -
                      colMeans(cbind(long[obsW == 0],lat[obsW == 0]))
      wt1 <- prop.table(1/prWEst_convnet[obsW == 1])
      wt0 <- prop.table(1/(1-prWEst_convnet[obsW == 0]))
      postDiff <- colSums(cbind(long[obsW == 1],lat[obsW == 1])*wt1) -
        colSums(cbind(long[obsW == 0],lat[obsW == 0])*wt0)

      tauHat_propensity = mean(  obsW*obsY/(prWEst_convnet) - (1-obsW)*obsY/(1-prWEst_convnet) )
      tauHat_propensityHajek = sum(  obsY*prop.table(obsW/(prWEst_convnet))) -
        sum(obsY*prop.table((1-obsW)/(1-prWEst_convnet) ))

      # sampling uncertainty only
      if(typeBoot == "SamplingOnly"){
        tauHat_propensity_vec = sapply(1:nBoot,function(b_){
          ib_ <- sample(1:length(obsY), length(obsY), replace = T)
          tauHat_ <-  mean(  obsW[ib_]*obsY[ib_]/(prWEst_convnet[ib_]) -
                               (1-obsW[ib_])*obsY[ib_]/(1-prWEst_convnet[ib_]) )
        })
        tauHat_propensityHajek_vec <- sapply(1:nBoot,function(b_){
          ib_ <- sample(1:length(obsY), length(obsY), replace = T)
          tauHat_ <-  sum(  obsY[ib_]*prop.table(obsW[ib_]/(prWEst_convnet[ib_]))) -
          sum(obsY[ib_]*prop.table((1-obsW[ib_])/(1-prWEst_convnet[ib_]) ))
        })
      }
    }

    print(  "Done with image confounding analysis!"  )
    return(    list(
      "tauHat_propensityHajek"  = tauHat_propensityHajek,
      "tauHat_propensity"  = tauHat_propensity,
      "tauHat_propensityHajek_se"  = sd(tauHat_propensityHajek_vec,na.rm=T),
      "tauHat_propensity_se"  = sd(tauHat_propensity_vec,na.rm=T),
      "tauHat_diffInMeans"  = mean(obsY[which(obsW==1)],na.rm=T) - mean(obsY[which(obsW==0)],na.rm=T),
      "SalienceX" = SalienceX,
      "prWEst_convnet" = prWEst_convnet,
      "nTrainableParameters" = nTrainable
    ) )
  }

    #!/usr/bin/env Rscript
    #' Visualizing matrices as heatmaps with correct north-south-east-west orientation
    #'
    #' A function for generating a heatmap representation of a matrix with correct spatial orientation.
    #'
    #' @usage
    #'
    #' image2( x )
    #'
    #' @param x (required) The numeric matrix to be visualized.
    #' @param xaxt (default = `""`) The x-axis label.
    #' @param yaxt (default = `""`) The y-axis label.
    #' @param main (default = `""`) The main figure label.
    #' @param cex.main (default = `1.`) The main figure label sizing factor.
    #' @param box (default = `F`) Should a box be plotted around the image?
    #'
    #' @return Returns a heatmap representation of the matrix, `x`, with correct north/south/east/west orientation.
    #'
    #' @examples
    #' #set seed
    #' set.seed(1)
    #'
    #' #Geneate data
    #' x <- matrix(rnorm(50*50), ncol = 50)
    #' diag(x) <- 3
    #'
    #' # create plot
    #' image2(x, main = "Example Text", cex.main = 2)
    #'
    #' @export
    #' @md
    #'
    image2 = function(x,xaxt=NULL,yaxt = NULL,main=NULL,cex.main = NULL,col.main = "black", box=F){
      image(x,
            axes = F,
            main = main,
            xaxs = "i",
            col.main = col.main,
            cex.main = cex.main)
      if(box == T){box()}
      if(!is.null(xaxt)){ axis(1, at = 0:(nrow(x)-1)/nrow(x)*1.04, tick=F,labels = (xaxt),cex.axis = 1,las = 1)  }
      if(!is.null(yaxt)){ axis(2, at = 0:(nrow(x)-1)/nrow(x)*1.04, tick=F,labels = rev(yaxt),cex.axis = 1,las = 2)  }
    }
    
    f2n <- function(.){as.numeric(as.character(.))}
    
    # zips two lists
    rzip<-function(l1,l2){  fl<-list(); for(aia in 1:length(l1)){ fl[[aia]] <- list(l1[[aia]], l2[[aia]]) }; return( fl  ) }
    
    # reshapes
    reshape_fxn_DEPRECIATED <- function(input_){
      ## DEPRECIATED
      tf$reshape(input_, list(tf$shape(input_)[1],
                              tf$reduce_prod(tf$shape(input_)[2:5])))
    }
    
    fixZeroEndings <- function(zr,roundAt=2){
      unlist( lapply(strsplit(as.character(zr),split="\\."),function(l_){
        if(length(l_) == 1){ retl <- paste(l_, paste(rep("0",times=roundAt),collapse=""),sep=".") }
        if(length(l_) == 2){
          retl <- paste(l_[1], paste(l_[2], paste(rep("0",times=roundAt-nchar(l_[2])),collapse=""),sep=""),
                        sep = ".") }
        return( retl  )
      }) ) }
    
    
    
    
    #' Get the spatial point of long/lat coordinates
    #'
    #' A function converts long/lat coordinates into a spatial points object defined by a coordinate reference system (CRS).
    #'
    #' @usage
    #'
    #' LongLat2CRS(long, lat, CRS_ref)
    #'
    #' @param long Vector of numeric longitudes.
    #' @param lat Vector of numeric latitudes.
    #' @param CRS_ref A CRS into which the long-lat point should be projected.
    #'
    #' @return Returns the long/lat location as a spatial point in the new CRS defined by `CRS_ref`
    #'
    #' @examples
    #' spatialPt <- LongLat2CRS(
    #'                   long = 49.932,
    #'                   lat = 35.432,
    #'                   CRS_ref = sp::CRS("+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"))
    #' @export
    #' @md
    #'
    LongLat2CRS <- function(long, lat, CRS_ref){
      library(sp)
      CRS_longlat <- CRS("+proj=longlat +datum=WGS84")
      point_longlat <- c(long,lat)
      point_longlat <- data.frame(ID = 1,
                                  X = as.numeric(point_longlat[1]),
                                  Y = as.numeric(point_longlat[2]))
      coordinates(point_longlat) <- c("X", "Y")
      proj4string(point_longlat) <- CRS_longlat
      SpatialTarget_utm <- SpatialPoints(spTransform(point_longlat, CRS_ref),
                                         CRS_ref)
      return( SpatialTarget_utm )
    }
    
    LongLat2CRS_extent <- function(point_longlat,
                                   CRS_ref,
                                   target_km_diameter = 10){
      CRS_longlat <- CRS("+proj=longlat +datum=WGS84")
      target_km <- 10
      point_longlat1 <- data.frame(ID = 1,
                                   X = as.numeric(point_longlat[1])-1/111*(target_km/2),
                                   Y = as.numeric(point_longlat[2])-1/111*(target_km/2))
      point_longlat2 <- data.frame(ID = 2,
                                   X = as.numeric(point_longlat[1])+1/111*(target_km/2),
                                   Y = as.numeric(point_longlat[2])+1/111*(target_km/2))
      point_longlat_mat <- rbind(point_longlat1,point_longlat2)
      coordinates(point_longlat_mat) <- c("X", "Y")
      proj4string(point_longlat_mat) <- CRS_longlat
      point_longlat_mat_ref <- SpatialPoints(spTransform(point_longlat_mat, CRS_ref), CRS_ref)
      return( raster::extent(point_longlat_mat_ref) )
    }
    
    #' Getting and saving geo-located images from a pool of .tif's
    #'
    #' A function that finds the image slice associated with the `long` and `lat` values, saves images by band (if `save_as = "csv"`) in save_folder.
    #'
    #' @usage
    #'
    #' GetAndSaveGeolocatedImages(long, lat, keys, tif_pool, save_folder)
    #'
    #' @param long Vector of numeric longitudes.
    #' @param lat Vector of numeric latitudes.
    #' @param keys The image keys associated with the long/lat coordinates.
    #' @param tif_pool A character vector specifying the fully qualified path to a corpus of .tif files.
    #' @param image_pixel_width An even integer specifying the pixel width (and height) of the saved images.
    #' @param save_folder (default = `"."`) What folder should be used to save the output? Example: `"~/Downloads"`
    #' @param save_as (default = `".csv"`) What format should the output be saved as? Only one option currently (`.csv`)
    #' @param lyrs (default = NULL) Integer (vector) specifying the layers to be extracted. Default is for all layers to be extracted.
    #'
    #' @return Finds the image slice associated with the `long` and `lat` values, saves images by band (if `save_as = "csv"`) in save_folder.
    #' The save format is: `sprintf("%s/Key%s_BAND%s.csv", save_folder, keys[i], band_)`
    #'
    #' @examples
    #'
    #' # Example use (not run)
    #' MASTER_IMAGE_POOL_FULL_DIR <- c("./LargeTifs/tif1.tif","./LargeTifs/tif2.tif")
    #' GetAndSaveGeolocatedImages(
    #'                        long = GeoKeyMat$geo_long,
    #'                        lat = GeoKeyMat$geo_lat,
    #'                        image_pixel_width = 500L,
    #'                        keys = row.names(GeoKeyMat),
    #'                        tif_pool = MASTER_IMAGE_POOL_FULL_DIR,
    #'                        save_folder = "./Data/Uganda2000_processed",
    #'                        save_as = "csv",
    #'                        lyrs = NULL)
    #'
    #' @import raster
    #' @export
    #' @md
    #'
    GetAndSaveGeolocatedImages <- function(
    long,
    lat,
    keys,
    tif_pool,
    image_pixel_width = 250L,
    save_folder = ".",
    save_as = "csv",
    lyrs = NULL){
      
      library(raster)
      RADIUS_CELLS <- (DIAMETER_CELLS <- image_pixel_width) / 2
      bad_indices <- c();observation_indices <- 1:length(long)
      counter_b <- 0 ; for(i in observation_indices){
        counter_b <- counter_b + 1
        if(counter_b %% 10 == 0){print(sprintf("Iter %s of %s",counter_b,length(observation_indices)))}
        SpatialTarget_longlat <- c(long[i],lat[i])
        # SpatialTarget_longlat <- c(32.821752, 1.827300)
        # rev(SpatialTarget_longlat)
        
        found_<-F;counter_ <- 0; while(found_ == F){
          counter_ <- counter_ + 1
          if(is.na(tif_pool[counter_])){
            found_ <- T; bad_ <- T
          }
          if(!is.na(tif_pool[counter_])){
            MASTER_IMAGE_ <- try(brick(tif_pool[counter_] ), T)
            SpatialTarget_utm <- LongLat2CRS(
              long = SpatialTarget_longlat[1],
              lat = SpatialTarget_longlat[2],
              CRS_ref = raster::crs(MASTER_IMAGE_))
            # check inverse of LongLat2CRS
            # SpatialTarget_longlat
            # SpatialPoints(spTransform(SpatialTarget_utm, CRS_longlat),CRS_longlat)
            
            # exact spatial target - alternative extraction method for pseudo rgb plotting
            if(T == F){
              my_extent <- LongLat2CRS_extent(SpatialTarget_longlat,raster::crs(MASTER_IMAGE_))
              
              cropped_raster <- raster::crop(MASTER_IMAGE_, y = my_extent)
              #raster::plotRGB(cropped_raster)
              matrix(getValuesBlock(cropped_raster[[band_]],
                                    row = 1, nrows = nrow(cropped_raster),
                                    col = 1, ncols = ncol(cropped_raster),
                                    format = "matrix", lyrs = 1L),
                     ncol = nrow(cropped_raster), byrow=T)
            }
            SpatialTargetCellLoc <- raster::cellFromXY(
              object = MASTER_IMAGE_,
              xy = SpatialTarget_utm)
            SpatialTargetRowCol <- raster::rowColFromCell(
              object = MASTER_IMAGE_,
              cell = SpatialTargetCellLoc )
            if(!is.na(sum(SpatialTargetRowCol))){found_<-T;bad_ <- F}
            if(counter_ > 1000000){stop("ERROR! Target not found anywhere in pool!")}
          }
        }
        if(bad_){
          print(sprintf("Bad at %s. Apparently, no .tif contains the reference point",i))
          bad_indices <- c(bad_indices,i)
        }
        if(!bad_){
          print(sprintf("Good at %s. Extracting and saving image", i))
          # available rows/cols
          rows_available <- nrow( MASTER_IMAGE_ )
          cols_available <- ncol( MASTER_IMAGE_ )
          
          
          # define start row/col
          start_row <- SpatialTargetRowCol[1,"row"] - RADIUS_CELLS
          start_col <- SpatialTargetRowCol[1,"col"] - RADIUS_CELLS
          
          # find end row/col
          end_row <- start_row + DIAMETER_CELLS
          end_col <- start_col + DIAMETER_CELLS
          
          # perform checks to deal with spilling over image
          if(start_row <= 0){start_row <- 1}
          if(start_col <= 0){start_col <- 1}
          if(end_row > rows_available){ start_row <- rows_available - DIAMETER_CELLS }
          if(end_col > cols_available){ start_col <- cols_available - DIAMETER_CELLS }
          
          for(iof in 0:0){
            if(is.null(lyrs)){lyrs <- 1:dim(MASTER_IMAGE_)[3] }
            band_iters <- ifelse(grepl(x = save_as, pattern ="csv"),
                                 yes = list(lyrs), no = list(1L) )[[1]]
            for(band_ in band_iters){
              if(iof > 0){
                start_row <- sample(1:(nrow(MASTER_IMAGE_)-DIAMETER_CELLS-1),1)
                start_col <- sample(1:(ncol(MASTER_IMAGE_)-DIAMETER_CELLS-1),1)
              }
              SpatialTargetImage_ <- getValuesBlock(MASTER_IMAGE_[[band_]],
                                                    row = start_row, nrows = DIAMETER_CELLS,
                                                    col = start_col, ncols = DIAMETER_CELLS,
                                                    format = "matrix", lyrs = 1L)
              #SpatialTargetImage_ <- matrix(SpatialTargetImage_,ncol = DIAMETER_CELLS, byrow = T)
              if(length(unique(c(SpatialTargetImage_)))<5){ bad_indices <- c(bad_indices,i) }
              check_ <- dim(SpatialTargetImage_) - c(DIAMETER_CELLS,DIAMETER_CELLS)
              if(any(check_ < 0)){print("WARNING: CHECKS FAILED"); browser()}
              # Tests:
              #pdf("~/Downloads/test.pdf");image2(SpatialTargetImage_, main = paste(round(SpatialTarget_longlat,4L),collapse = ",") );dev.off()
              # SpatialTarget_longlat
              if(grepl(x = save_as, pattern ="tif")){
                # in progress
              }
              if(grepl(x = save_as, pattern ="csv")){
                if(iof == 0){
                  data.table::fwrite(file = sprintf("%s/Key%s_BAND%s.csv",
                                                    save_folder, keys[i], band_),
                                     data.table::as.data.table(SpatialTargetImage_))
                }
                #if(iof > 0){
                #data.table::fwrite(file = sprintf("./Data/Uganda2000_processed_comparisons/Key%s_%s_BAND%s.csv",
                #row.names(GeoKeyMat)[i],iof,band_),
                #data.table::as.data.table(SpatialTargetImage_)) }
              }
            }
          }
        }
      }
    }
    
    
    r2const <- function(x, dtype){
      if("tensorflow.tensor" %in% class( x )){ }
      if(!"tensorflow.tensor" %in% class( x )){ x <- tf$constant(  x, dtype = dtype  ) }
      return( x )
    }
    


# call_CI_Conf_emb_5k_annual.R
# Desc:  Calls Causal Image Confounding over DHS points, using 5 satellite bands,
#        Uses annual observations, annual 5k images, and randomized embeddings
#        Uses per-capital nighlights instead of avg nightlights
#        Includes country-level variables other than country
#        Generates an xy plot comparing salience of tab conf w & w/o images
#         Generates a tfrecord file for the run
#		  Shuffles the data before using it.
#     Includes an agglomeration variable
#     Retires logistic reg; remove gridlines from plots; seed is sector
#     Handles SalienceX_se variables.
#     IWI measured 3 years post-project start	
#     Exclude treatments < 2001 (missing population density)
#     Confounder variable if other funder had a treatment in same year								 
library(causalimages)
library(dplyr)
library(tensorflow)

rm(list=ls())
setwd("/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb")
args <- commandArgs(trailingOnly = TRUE)

# The first command line argument should be funder_sector (like both_110, wb_110, ch_110)
fund_sect_param <- args[1]
run <- args[2]
iterations <- as.integer(args[3])
time_approach <- args[4]

#uncomment to test
#fund_sect_param <- "wb_110"
# fund_sect_param <- "ch_430"
# run <- "emb_5k_annual"
# iterations <- 1000
# time_approach <- "annual"   #other option: "collapsed"

################################################################################
# Initial setup, parameter processing, reading input files 
################################################################################
results_dir <- paste0("./results/",run,"/")
#create the results directory for this run if it doesn't exist already
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}
sector_param <- sub(".*_(\\d+).*", "\\1", fund_sect_param)
funder_param <- sub("(wb|ch).*", "\\1", fund_sect_param)

##### read confounder and treat/control data from files
dhs_confounders_df <- read.csv("./data/interim/dhs_5k_confounders.csv") %>% 
  select(-year)  #remove survey year column that could be confused with oda year

dhs_t_df <- read.csv("./data/interim/dhs_treat_control_5k_annual.csv") %>% 
  filter(sector==sector_param & funder==funder_param & 
           dhs_id %in% dhs_confounders_df$dhs_id) 
#exclude DHS points where confounder data not available 

#limit to dhs_id and iso3 for use in join below
dhs_iso3_df <- dhs_confounders_df %>% 
  distinct(dhs_id,iso3)

#identify countries where funder is operating in this sector
funder_sector_iso3 <- dhs_confounders_df %>% 
  filter(dhs_id %in% (dhs_t_df %>% 
                        pull(dhs_id))) %>% 
  distinct(iso3)

#get controls, limiting to countries and year where funder operated in sector
dhs_c_df <- read.csv("./data/interim/dhs_treat_control_5k_annual.csv") %>% 
  filter(sector==sector_param & funder=="control") %>% 
  inner_join(dhs_iso3_df,by="dhs_id") %>% 
  filter(iso3 %in% funder_sector_iso3$iso3 & 
           dhs_id %in% dhs_confounders_df$dhs_id &
           #exclude DHS points where confounder data not available 
           start_year %in% dhs_t_df$start_year) 

#define variable order and names for boxplots and dropped cols variables
var_order_all <- c("iwi_est_post_oda","log_pc_nl_pre_oda","log_avg_pop_dens",
               "log_avg_min_to_city","agglomeration",
               "log_dist_km_to_gold","log_dist_km_to_gems",        
               "log_dist_km_to_dia","log_dist_km_to_petro", 
               "leader_birthplace","log_trans_proj_cum_n",
               "log_3yr_pre_conflict_deaths",
               "polity2","log_gdp_per_cap_USD2015","country_gini","landsat57",
               "landsat578","treated_both_funders")
var_labels_all <- c("Wealth (est, t+3)","Nightlights per capita (t-1,log)","Pop Density (t-1,log)",
                "Minutes to City (2000,log)","Agglomeration (t-1)","Dist to Gold (km,log)",
                "Dist to Gems (km,log)","Dist to Diam (km,log)",
                "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                "Conflict deaths (t-1,log)",
                "Country Polity2 (t-1)","Cntry GDP/cap (t-1,log)","Country gini (t-1)",
                "Landsat 5 & 7", "Landsat 5,7,& 8","Treated Other Funder")

################################################################################
# Function called by AnalyzeImageConfounding to read images 
################################################################################
acquireImageRepFromDisk <- function(keys,training = F){
  imageHeight = 167  # 30m/pixel
  imageWidth = 167
  NBANDS = 5
  #will use bands BGR,NIR,SWIR1
  #layers depend on aid year. tif has 84 layers, 6 for each year in this order:
  #BLUE, GREEN, RED, NIR, SWIR1, SWIR2 - use first 5
  
  # initialize an array shell to hold image slices
  array_shell <- array(NA,dim = c(1L,imageHeight,imageWidth,NBANDS))

  # iterate over keys:
  # -- images are referenced to keys
  # -- keys are referenced to units (to allow for duplicate images uses)
  array_ <- sapply(keys,function(key_){
    #comment out to test 
    #key_ = "./data/dhs_tifs_annual/zambia_2007/00221.tif2001"

    oda_start_year <- sub(".*\\.tif(.*)", "\\1", key_)
    image_file <- sub("(.*\\.tif).*", "\\1", key_)

    #oda_start_year <- 2002  #comment out to test
    #determine which year's images to get

    #select bands of the PREVIOUS (pre-project) period
    oda_start_year <- as.integer(oda_start_year)
    bands_to_select <- case_when(
      oda_start_year==2001 ~ 1:5,
      oda_start_year==2002 ~ 7:11,
      oda_start_year==2003 ~ 13:17,
      oda_start_year==2004 ~ 19:23,
      oda_start_year==2005 ~ 25:29,
      oda_start_year==2006 ~ 31:35,
      oda_start_year==2007 ~ 37:41,
      oda_start_year==2008 ~ 43:47,
      oda_start_year==2009 ~ 49:53,
      oda_start_year==2010 ~ 55:59,
      oda_start_year==2011 ~ 61:65,
      oda_start_year==2012 ~ 67:71,
      oda_start_year==2013 ~ 73:77,
      oda_start_year==2014 ~ 79:83
      )

    # iterate over all image bands
    for(i in 1:NBANDS) {
      band_ <- bands_to_select[i]
      im <- terra::rast(image_file,
                        lyrs=paste0(gsub(pattern=".*/(\\d{5})\\.tif$","\\1", x=image_file)
                                    ,"_",band_))
      #rescale to original setting for RGB printing
      im <- im/.0000275
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

################################################################################
# Set tabular confounding variables based on start year
################################################################################
treat_count <- nrow(dhs_t_df) 
control_count <- nrow(dhs_c_df)

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
               " Processing ",fund_sect_param,
               ", treat n:",treat_count,
               ", control n: ",control_count,
               ", run: ",run,
               ", iterations: ",iterations
  ))
  
  ##############################################################################
  # combine treated & controls into same dataframe, join with confounders,
  # and adjust to be appropriate for start year  
  ##############################################################################
  obs_year_df <- rbind(
    dhs_t_df %>% 
       mutate(treated=1) %>% 
       select(dhs_id,start_year,treated,proj_count,treated_both_funders),
    dhs_c_df %>% 
       mutate(treated=0) %>% 
       select(dhs_id,start_year,treated,proj_count,treated_both_funders)
    ) %>% 
    left_join(dhs_confounders_df,by="dhs_id") %>% 
    mutate(
      iwi_est_post_oda = case_when(
        start_year %in% 2000:2001 ~ iwi_2002_2004_est,
        start_year %in% 2002:2004 ~ iwi_2005_2007_est,
        start_year %in% 2005:2007 ~ iwi_2008_2010_est,
        start_year %in% 2008:2010 ~ iwi_2011_2013_est,
        start_year %in% 2011:2013 ~ iwi_2014_2016_est,
        start_year == 2014 ~ iwi_2017_2019_est),
      log_dist_km_to_gold = case_when(
        start_year %in% 2000:2001 ~ log_dist_km_to_gold_pre2001,
        start_year > 2001 ~ log_dist_km_to_gold_2001),
      log_dist_km_to_petro = if_else(
        start_year < 2003, 
        log_dist_km_to_petro_2000_2002,log_dist_km_to_petro_2003),
      #set dummy variables for the combination of satellite images in pre-project images
      #Landsat 5 only in images from 1990:1998 - won't include this column to avoid collinearity
      #Landsat 5&7 in images from    1999:2010 
      landsat57 = if_else(start_year %in% 2002:2013,1,0),
      #Landsat 5,7, & 8 in images from 2011:2013
      landsat578 = if_else(start_year %in% 2014:2016,1,0)
      #Landsat 7&8 in images from 2014:2019 - we aren't using any of these
      ) %>% 
    #set per cap nl to year prior to aid project
    rowwise() %>% mutate(
      log_pc_nl_pre_oda = get(paste0("log_pc_nl_",start_year - 1))
    ) %>% ungroup() %>%
    #set population density to year prior to earliest aid project
    rowwise() %>% mutate(
      log_avg_pop_dens = get(paste0("log_avg_pop_dens_",start_year - 1))
    ) %>% ungroup() %>% 
    #set agglomeration to year prior to aid project
    rowwise() %>% mutate(
      agglomeration = get(paste0("agglom_",start_year - 1))
    ) %>% ungroup() %>%
    #set leader_birthplace based on year prior to earliest aid project 
    rowwise() %>% mutate(
      leader_birthplace = get(paste0("leader_", start_year - 1))
    ) %>% ungroup() %>% 
    #set conflict deaths to 3 year pre-project sum 
    rowwise() %>%  mutate(
      log_3yr_pre_conflict_deaths = get(paste0("log_deaths", 
                                               start_year - 3,
                                               "_",
                                               start_year - 1))
    ) %>% ungroup() %>%  
    #set loan-based transport projects based on year prior to earliest aid project 
    rowwise() %>% mutate(
      log_trans_proj_cum_n = get(paste0("log_trans_proj_cum_n_",
                                        start_year - 1))
    ) %>% ungroup()  
  
  #join to country-level parameters, which are year specific
  country_confounders_df <- read.csv("./data/interim/country_confounders.csv") %>% 
    select(-country) %>% 
    mutate(year=year+1)  #add 1 to year for join below, to get 1 year pre-project
  
  run_df <- obs_year_df %>% 
    left_join(country_confounders_df,
              by=join_by(iso3, start_year == year))

  #create input_df and write to file
  pre_shuffle_df <- run_df %>% 
    select(dhs_id, country, iso3, lat, lon, treated, treated_both_funders,
           start_year, image_file_annual, iwi_est_post_oda,
           log_pc_nl_pre_oda, log_avg_min_to_city, log_avg_pop_dens, agglomeration,
           log_3yr_pre_conflict_deaths, log_trans_proj_cum_n, leader_birthplace, log_dist_km_to_gold,
           log_dist_km_to_gems, log_dist_km_to_dia, log_dist_km_to_petro,
           log_gdp_per_cap_USD2015, country_gini, polity2, landsat57, landsat578) %>% 
    rename(cnty = country) %>% 
    mutate(cnty = stringr::str_to_title(cnty))
  
  #shuffle data to reorder it before use; set.seed call makes it reproducible
  set.seed(sector_param)
  input_df <- pre_shuffle_df[sample(x=1:nrow(pre_shuffle_df),size=nrow(pre_shuffle_df),replace=FALSE),]
  
  write.csv(input_df, paste0("./data/interim/input_",run,"_",fund_sect_param,".csv"),row.names = FALSE)
  
  if (nrow(input_df[!complete.cases(input_df),]) > 0) {
    print(paste0("Stopping because incomplete cases.  See ./data/interim/input_",
                 run,"_",fund_sect_param,".csv"))
  } else {
    conf_matrix <- cbind(
      as.matrix(data.frame(
        "start_year"                 =input_df$start_year,
        "start_year_squared"         =input_df$start_year^2,
        "treated_both_funders"       =input_df$treated_both_funders,
        "log_pc_nl_pre_oda"          =input_df$log_pc_nl_pre_oda,           #scene level
        "log_avg_min_to_city"        =input_df$log_avg_min_to_city,         #scene level
        "log_avg_pop_dens"           =input_df$log_avg_pop_dens,            #scene level
        "agglomeration"              =input_df$agglomeration,               #scene level
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
      )),
      #multiple columns for country variables
      model.matrix(~ cnty - 1, input_df)
    )
    
    #remove any columns that have 0 standard deviation before passing to function
    before_cols <-  colnames(conf_matrix)
    conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 
    dropped_cols <- setdiff(before_cols, colnames(conf_matrix))
    dropped_labels <- paste(var_labels_all[match(setdiff(before_cols, colnames(conf_matrix)),var_order_all)],collapse="; ")
    if (dropped_labels != "") {
      print(paste("Dropped for 0 SD: ", dropped_labels))
    }

    #remove no-variation columns from the variables that control later figures
    var_order <- setdiff(var_order_all,dropped_cols)
    var_labels <- setdiff(var_labels_all,
                          var_labels_all[match(setdiff(before_cols, colnames(conf_matrix)),var_order_all)])
    
    #cleanup unneeded objects in memory before calling function
    rm(dhs_c_df,dhs_confounders_df,dhs_iso3_df,dhs_t_df,funder_sector_iso3,obs_year_df,
       run_df, pre_shuffle_df)
    
    ################################################################################
    # Generate tf_records file for this sector/funder/time_approach if not present 
    ################################################################################
    tf_rec_filename <- paste0("./data/interim/tfrecords/",fund_sect_param,"_",
                              time_approach,"_5k_s3.tfrecord")
    
    if (!file.exists(tf_rec_filename)) {
      print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                   " Start creating tfrecord file: ",tf_rec_filename))
      
      causalimages::WriteTfRecord(file = tf_rec_filename,
                                  imageKeysOfUnits = paste0(input_df$image_file_annual,
                                                            input_df$start_year),
                                  acquireImageFxn = acquireImageRepFromDisk
      )
      print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                   " Finished creating tfrecord file: ",tf_rec_filename))
    }

    ###############################
    # call AnalyzeImageConfounding
    ###############################
    ImageConfoundingAnalysis <- AnalyzeImageConfounding(
      obsW = input_df$treated,
      obsY = input_df$iwi_est_post_oda,  
      X = conf_matrix,
      long = input_df$lon,
      lat = input_df$lat,
      #concatenate the image file location and oda start year into a single keys parameter
      imageKeysOfUnits = paste0(input_df$image_file_annual,input_df$start_year), 
      #acquireImageFxn = acquireImageRepFromDisk,
      file = tf_rec_filename,
      samplingType = "balancedTrain",
      nSGD = iterations,
      nDepthHidden_conv = 1L, nDepthHidden_dense = 1L, maxPoolSize = 2L, strides = 2L, kernelSize = 9L,
      modelClass = "embeddings",
      nBoot=30L,
      nFilters = 50L,
      nEmbedDim = 128L,	
      figuresPath = results_dir, # figures saved here
      plotBands=c(3,2,1),  #red, green, blue
      figuresTag = paste0(fund_sect_param,"_",run,"_i",iterations),
      tagInFigures = T,
      conda_env = NULL, # conda env to try to activate
      conda_env_required = F
    )

    ica_df <- data.frame(t(unlist(ImageConfoundingAnalysis)))
    output_df <- cbind(data.frame(run,fund_sect_param,treat_count,control_count,
                                  dropped_labels,
                                  ica_df))
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                 " Writing to ",results_dir,"ICA_",fund_sect_param,"_",run,"_i"
                 ,iterations,".csv"))
    write.csv(output_df,paste0(results_dir,"ICA_",fund_sect_param,"_",run,"_i",
                               iterations,".csv"),row.names = FALSE)
    ############################################################################
    # use output to identify 3 least/most likely locations to receive aid
    ############################################################################   
    library(tidyr)   
    
    ica_long_df <-  ica_df %>% 
      pivot_longer(cols=starts_with("prW_est"),names_to="col_index", 
                   names_prefix="prW_est",values_to="prW_est") %>% 
      mutate(col_index = as.integer(col_index))
    
    input_r_df <- input_df %>% 
      mutate(rnum=row_number())
    
    #join the input and output and sort by treatment propensity
    ica_long2_df <- ica_long_df %>% 
      inner_join(input_r_df,join_by(col_index==rnum)) %>% 
      arrange(prW_est)
    
    least_likely_df <- ica_long2_df %>% 
      filter(treated==0) %>% 
      slice(1:3) %>% 
      mutate(rank=row_number())
    
    most_likely_df <- ica_long2_df %>% 
      filter(treated==1) %>% 
      tail(3) %>% 
      arrange(desc(row_number())) %>% 
      mutate(rank=row_number())
    
    #print these to log
    print(paste("Most likely treated for", fund_sect_param, "run:", run))
    print(most_likely_df %>% 
            select(prW_est,col_index,image_file_annual,start_year))
    print(paste("Least likely treated for", fund_sect_param, "run:", run))
    print(least_likely_df %>% 
            select(prW_est,col_index,image_file_annual,start_year))
    
    
    ############################################################################
    # generate boxplots for this run
    ############################################################################
    library(ggplot2)
  
    long_funder <- case_when(
      startsWith(fund_sect_param, "ch") ~ "China",
      startsWith(fund_sect_param, "wb") ~ "World Bank",
      startsWith(fund_sect_param, "both") ~ "Both China & World Bank"
    )

    sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
      mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")"))

    sector_name <- sector_names_df %>%
      filter(ad_sector_codes==sector_param) %>%
      pull(sec_pre_name)

    # Convert to long format for boxplots
    long_input_df <- input_df %>%
      select(treated,all_of(var_order)) %>%
      tidyr::pivot_longer(c(-treated),names_to="variable_name", values_to="value")

    sub_l1 <- paste("Funder:",long_funder,"     Sector:", sector_name)
    sub_l2 <- ifelse(nzchar(dropped_labels),
                     paste0("Dropped due to no variation: ", dropped_labels),
                     "")
    
    combined_boxplot <- ggplot(long_input_df, aes(x = factor(treated), y = value)) +
      geom_boxplot() +
      labs(title = "Distribution of Wealth Outcome and Confounders for Treated and Control DHS locations",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = paste0("Treatment/Control: 0:control (n ",control_count,"), 1:treated (n ",treat_count,")"),
           y = "Value",
           color = "Highest Pr(T=1)",
           fill = "Lowest Pr(T=1)") +
      facet_wrap(~ variable_name, scales = "free") +
      facet_wrap(~ factor(variable_name, levels = var_order, labels = var_labels), scales = "free") +
      theme_bw()  +
      theme(panel.grid = element_blank()) 

    ggsave(paste0(results_dir,fund_sect_param,"_20boxplots_",run,".pdf"),
           combined_boxplot,
           width=10, height = 8, dpi=300,
           bg="white", units="in")
    
    #############################################################################
    ##### create a line plot comparing confounders to outcome variable
    #############################################################################
    # Set the treated color based on funder
    treat_color <- case_when(
      startsWith(fund_sect_param, "ch") ~ "indianred1",
      startsWith(fund_sect_param, "wb") ~ "mediumblue",
      startsWith(fund_sect_param, "both") ~ "blueviolet"
    )

    #Convert to longer format for density plots, leaving outcome as separate column
    hybrid_input_df <- input_df %>%
      select(treated,all_of(var_order)) %>% 
      pivot_longer(c(-treated,-iwi_est_post_oda),names_to="variable_name", values_to="value")  
    
    outcome_confounders_plot <- ggplot(hybrid_input_df, aes(x = iwi_est_post_oda, y=value, color = factor(treated))) +
      geom_point(alpha = 0.3) +
      facet_wrap(~ variable_name, scales = "free_y", ncol = 3) +
      facet_wrap(~ factor(variable_name, levels = var_order, labels = var_labels), scales = "free") +
      labs(title = "Confounders vs. Estimated wealth for Treated and Control DHS locations",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Estimated Wealth Index 3 years post-project",
           y = "Value",
           color="Treated") +
      scale_color_manual(values = c("gray60", treat_color),
                         labels = c("Control", "Treated")) +
      theme_bw()  +
      theme(panel.grid = element_blank())
    
    ggsave(paste0(results_dir,fund_sect_param,"_30conf_iwi_",run,".png"),
           outcome_confounders_plot,
           width=10, height = 8, dpi=300,
           bg="white", units="in")

    ##########################################################################
    ##### generate treatment/control map
    ##########################################################################
    library(tmap)
    tmap_options(check.and.fix = TRUE)
    projection <- "ESRI:102023"
    
    #convert DHS points df to sf object
    input_sf <- sf::st_as_sf(input_df, coords=c("lon","lat"),crs="EPSG:4326")  %>%
      sf::st_transform(crs=sf::st_crs(projection))

    ###############################################################
    #### Load administrative borders and ISO list excluding islands
    ###############################################################
    africa_map_isos_df <- read.csv("./data/interim/africa_map_isos.csv")
    
    country_borders <-  sf::read_sf("./data/country_regions/gadm28_adm0.shp") %>%
      filter(ISO %in% africa_map_isos_df$iso3)
    sf::st_crs(country_borders) = "EPSG:4326"
    country_borders <- sf::st_transform(country_borders,crs=sf::st_crs(projection))
    country_borders <- sf::st_make_valid(country_borders)
    
    ########################
    #### Generate map
    ########################
    treat_control_map <- tm_shape(country_borders) +
      tm_borders(lwd=2) +
      tm_shape(input_sf[input_sf$treated == 0, ]) +  
      tm_dots(size=.3, col="gray80", alpha=.3) +
      tm_shape(input_sf[input_sf$treated == 1, ]) +  
      tm_dots(size=.5, col=treat_color, alpha=.3) +
      tm_add_legend(type = "fill"
                    , col = c(treat_color,"gray80")
                    , labels = c(paste0("Treated (n ",treat_count,")"),
                                 paste0("Control (n ",control_count,")")))  +
      tm_add_legend(type="symbol",
                    col=c(treat_color,"gray39"),
                    shape=c(24,25),
                    labels=c("3 Highest Pr(T=1)","3 Lowest Pr(T=1)")) +
      tm_layout(main.title.size=1,
                main.title = paste0(long_funder,": ",sector_name,"\nTreatment and Control Locations (2001-2014)"),
                main.title.position=c("center","top"),
                legend.position = c("left", "bottom"),
                legend.text.size = 1,
                legend.width=-1,
                frame = T ,
                legend.outside = F, 
                outer.margins = c(0, 0, 0, 0), 
                legend.outside.size = .25
      )
    
    tmap_save(treat_control_map,paste0(results_dir,fund_sect_param,"_10map_",run,".png"))
  

    ############################################################################
    ##### ridge regression for treatment probabilities with tabular confounders
    ############################################################################
    library(glmnet)

    # use cross-validation to choose lambda 
    cv_model_ridge <- cv.glmnet(x=scale(conf_matrix), y=input_df$treated,
                                family = "binomial", alpha = 0)
    best_lambda_ridge <- cv_model_ridge$lambda.min
    
    # Fit model with best lambda
    ridge_model_best <- glmnet(x=scale(conf_matrix), y=input_df$treated, 
                               family = "binomial", alpha = 0, 
                               lambda = best_lambda_ridge)
    
    ridge_coeffs_df <- broom::tidy(ridge_model_best)
    treat_prob_log_r_df <- ridge_coeffs_df %>%
      rename(ridge_est=estimate)
    
    # Predict probabilities
    ridge_predicted_probs <- predict(ridge_model_best, newx = scale(conf_matrix),
                                     type = "response")
    
    # Create a data frame with predicted probabilities, and actual treatment status
    ridge_result_df <- data.frame(predicted_probs = ridge_predicted_probs, 
                                  treated = input_df$treated)
    
    ridge_conf_density <- ggplot(ridge_result_df, aes(x = s0, fill = factor(treated))) +
      geom_density(alpha = 0.5) +
      labs(title = "Ridge regression: Density Plot for\nEstimated Pr(T=1 | Tabular Confounders)",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Predicted Treatment Propensity",
           y = "Density",
           fill="Status") +
      scale_fill_manual(values = c("gray80", treat_color),
                        labels = c("Control", "Treated")) +
      theme_bw()  +
      theme(panel.grid = element_blank())
    
    
    #save
    ggsave(paste0(results_dir,fund_sect_param,"_50ridge_prop_",run,".pdf"),
           ridge_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")
    
    ############################################################################
    ##### add SalienceX & .se to df, save, and plot ridge and SalienceX values
    ############################################################################    
    #extract tabular confounder salience values from image confounding output
    tab_conf_salience_df <- ica_df %>%
      select(starts_with("SalienceX"))  %>%
      pivot_longer(cols=everything()) %>% 
      separate_wider_delim(name,delim=".",names=c("measure","term")) %>% 
      pivot_wider(names_from = measure, values_from=value) %>% 
      mutate(SalienceX_tscore = abs(SalienceX/SalienceX_se),
             SalienceX_sig = ifelse(SalienceX_tscore >= 1.96, "*",""))
    
    #join to dataframe with ridge output
    tab_conf_compare_df <-  treat_prob_log_r_df %>% 
      right_join(tab_conf_salience_df, by="term") %>% 
      rename(Salience_AIC = SalienceX)
    
    #write to file
    write.csv(tab_conf_compare_df,
              paste0(results_dir,fund_sect_param,"_tab_conf_compare_", run,".csv"),
              row.names = FALSE)
    
    #plot these
    #determine the limits of the plot
    max_abs_value <- ceiling(max(c(abs(tab_conf_compare_df$ridge_est),
                                   abs(tab_conf_compare_df$Salience_AIC)),
                                 na.rm=T)
    )
    
    treat_sig_color <- ifelse(tab_conf_compare_df$SalienceX_sig=="","gray80",
                              treat_color)
    tab_est_images <- tab_conf_compare_df %>% 
      mutate(term=case_match(term,
                             "log_pc_nl_pre_oda" ~ "percap_nightlights",
                             "log_avg_min_to_city" ~ "min_to_city",
                             "log_avg_pop_dens" ~ "pop_dens",
                             "log_3yr_pre_conflict_deaths" ~ "conflict_deaths",
                             "log_trans_proj_cum_n" ~ "ch_transp_projs",
                             "log_dist_km_to_gold" ~ "dist_to_gold",
                             "log_dist_km_to_gems" ~ "dist_to_gems",
                             "log_dist_km_to_dia" ~ "dist_to_dia",
                             "log_dist_km_to_petro" ~ "dist_to_petro",
                             .default=term)) %>% 
      #remove country variables from plot
      filter(!grepl("cnty",term)) %>% 
      ggplot(aes(x = ridge_est, y = Salience_AIC, label = term)) +
      geom_point(color=treat_sig_color) +
      ggrepel::geom_text_repel(box.padding = 1,max.overlaps=Inf,color=treat_sig_color,
                               segment.color="gray80") + 
      geom_vline(xintercept=0,color="gray80") +
      geom_hline(yintercept=0, color="gray80") +
      geom_abline(intercept=0, slope=1, linetype="dashed",color="gray80") +
      labs(title = "Tabular confounder estimates with and without images",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Tabular confounders only: Ridge estimate",
           y = "Salience with Image Confounding") +   
      coord_fixed(ratio=1,xlim=c(-1*max_abs_value,max_abs_value),
                  ylim=c(-1*max_abs_value,max_abs_value)) +
      theme_bw()  +
      theme(panel.grid = element_blank())
    
    #save
    ggsave(paste0(results_dir,fund_sect_param,"_90xy_tab_conf_images_",run,".pdf"),
           tab_est_images,
           width=6, height = 6, dpi=300,
           bg="white", units="in")
    
    
    ############################################################################
    #print these messages again to be at the end of the logfile
    ############################################################################
    if (dropped_labels != "") {
      print(paste("Dropped for 0 SD: ", dropped_labels))
    }
  }
}

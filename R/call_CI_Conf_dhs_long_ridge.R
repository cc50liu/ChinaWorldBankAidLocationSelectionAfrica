# call_CI_Conf_dhs_long.R
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
run <- args[2]
iterations <- as.integer(args[3])

#uncomment to test
#fund_sect_param <- "both_140"
# fund_sect_param <- "wb_110"
#fund_sect_param <- "ch_140"
#run <- "v1long"
#iterations <- 1000

################################################################################
# Initial setup, parameter processing, reading input files 
################################################################################
sector_param <- sub(".*_(\\d+).*", "\\1", fund_sect_param)
funder_param <- sub("(wb|ch|both).*", "\\1", fund_sect_param)

##### read confounder and treat/control data from files
dhs_confounders_df <- read.csv("./data/interim/dhs_confounders.csv") %>% 
  select(-year)  #remove survey year column that could be confused with oda year

dhs_t_df <- read.csv("./data/interim/dhs_treat_control_long.csv") %>% 
  filter(sector==sector_param & funder==funder_param & 
           dhs_id %in% dhs_confounders_df$dhs_id) 
           #exclude 5 DHS points where confounder data not available 

dhs_iso3_df <- dhs_confounders_df %>% 
  distinct(dhs_id,iso3)

funder_sector_iso3 <- dhs_confounders_df %>% 
  filter(dhs_id %in% (dhs_t_df %>% 
                        pull(dhs_id))) %>% 
  distinct(iso3)

#limit controls to countries where this funder is operating in this sector
dhs_c_df <- read.csv("./data/interim/dhs_treat_control_long.csv") %>% 
  filter(sector==sector_param & funder=="control") %>% 
  inner_join(dhs_iso3_df,by="dhs_id") %>% 
  filter(iso3 %in% funder_sector_iso3$iso3 & 
         dhs_id %in% dhs_confounders_df$dhs_id) 
         #exclude 5 DHS points where confounder data not available )
    

#define variable order and names for boxplots and dropped cols variables
var_order <- c("iwi_est_post_oda","log_avg_nl_pre_oda","log_avg_pop_dens",
               "log_avg_min_to_city",
               "log_dist_km_to_gold","log_dist_km_to_gems",        
               "log_dist_km_to_dia","log_dist_km_to_petro", 
               "leader_birthplace","log_trans_proj_cum_n",
               "log_3yr_pre_conflict_deaths",
               "polity2","log_gdp_per_cap_USD2015","country_gini","landsat57",
               "landsat578")
var_labels <- c("Wealth (est, t+3)","Nightlights (t-3,log)","Pop Density (t-1,log)",
                "Minutes to City (2000,log)","Dist to Gold (km,log)",
                "Dist to Gems (km,log)","Dist to Diam (km,log)",
                "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                "Conflict deaths (t-1,log)",
                "Country Polity2 (t-1)","Cntry GDP/cap (t-1,log)","Country gini (t-1)",
                "Landsat 5 & 7", "Landsat 5,7,& 8")

################################################################################
# Function called by AnalyzeImageConfounding to read images 
################################################################################
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
  
  # initialize an array shell to hold image slices
  array_shell <- array(NA,dim = c(1L,imageHeight,imageWidth,NBANDS))

  # iterate over keys:
  # -- images are referenced to keys
  # -- keys are referenced to units (to allow for duplicate images uses)
  array_ <- sapply(keys,function(key_){
    #comment out to test 
    #key_ = "./data/dhs_tifs/angola_2006/00000.tifNA"
    #key_ = "./data/dhs_tifs/angola_2006/00000.tif2001"

    oda_start_year <- sub(".*\\.tif(.*)", "\\1", key_)
    image_file <- sub("(.*\\.tif).*", "\\1", key_)
        
    #oda_start_year <- 2002  #comment out to test
    #determine which year's images to get
    if (oda_start_year=="NA") {
      # don't expect NA's, but if happens, default to 1996:1998 (bands 17:21)
      bands_to_select <- c(17:21)
    } else {
      #select bands of the PREVIOUS (pre-project) period
      oda_start_year <- as.integer(oda_start_year)
      bands_to_select <- case_when(
                          oda_start_year %in% 1999:2001 ~ 17:21,
                          oda_start_year %in% 2002:2004 ~ 25:29,
                          oda_start_year %in% 2005:2007 ~ 33:37,
                          oda_start_year %in% 2008:2010 ~ 41:45,
                          oda_start_year %in% 2011:2013 ~ 49:53,
                          oda_start_year %in% 2014:2016 ~ 57:61)
    }

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
################################################################################
# Give control points same distribution of start/end years as treated points
################################################################################
treat_year_props <- dhs_t_df %>%
  group_by(image_group,start_year,max_end_year) %>%
  summarize(count = n(), .groups = "drop") %>%
  ungroup() %>%
  group_by(image_group) %>% 
  mutate(image_group_count= sum(count)) %>% 
  mutate(proportion = count / image_group_count) %>% 
  ungroup()

control_before <- dhs_c_df %>% 
  group_by(image_group,start_year,max_end_year) %>% 
  count() 

control_props <- control_before %>% 
  left_join(treat_year_props, by="image_group",multiple="all") %>% 
  select(-start_year.x, -max_end_year.x) %>% 
  rename(treat_start_year=start_year.y,
         treat_max_end_year=max_end_year.y,
         treat_n=count,
         cntl_n=n) %>% 
  mutate(desired_controls = round(cntl_n * proportion,0))
#example: ch_140
# image_group cntl_n treat_start_year treat_max_end_year treat_n image_group_count proportion desired_controls
# <chr>        <int>            <int>              <int>   <int>             <int>      <dbl>            <dbl>
# 1 1999:2001     2565               NA                 NA      NA                NA    NA                    NA
# 2 2002:2004     1684             2002               2003       2                26     0.0769              130
# 3 2002:2004     1684             2003               2003      24                26     0.923              1554
# 4 2005:2007     2171             2006               2007      13                38     0.342               743
# 5 2005:2007     2171             2007               2008       1                38     0.0263               57
# 6 2005:2007     2171             2007               2010      24                38     0.632              1371
# 7 2008:2010     1977             2008               2009     123               167     0.737              1456
# 8 2008:2010     1977             2009               2010      44               167     0.263               521
# 9 2011:2013     2041             2012               2013      71               116     0.612              1249
# 10 2011:2013     2041             2013               2014      45               116     0.388               792
# 11 2014:2016     2497               NA                 NA      NA                NA    NA                    NA

  
#remove control rows for time periods that had no treatments
dhs_c_image_group_df <- dhs_c_df %>% 
  filter(!image_group %in% (control_props %>% 
                              filter(is.na(treat_n)) %>% 
                              pull(image_group))) 

control_props2 <- control_props %>% 
  filter(!is.na(treat_n))
  
set.seed(1234)
for (i in 1:nrow(control_props2)) {
  #i=5  #uncomment to test

  # Randomly select control points to be assigned loop's start_year and max_end_year
  dhs_ids_to_update <- dhs_c_image_group_df %>%
    filter(image_group==control_props2$image_group[i] & 
             is.na(start_year)) %>% 
    slice_sample(n=control_props2$desired_controls[i], replace = FALSE) %>% 
    select(dhs_id, image_group) %>% 
    mutate(target=paste0(dhs_id,"_",image_group))
  
  # Update randomly selected control dhs_ids with year of this loop
  dhs_c_image_group_df <- dhs_c_image_group_df %>%
    mutate(start_year = ifelse(paste0(dhs_id,"_",image_group) %in% dhs_ids_to_update$target,
                          control_props2$treat_start_year[i], 
                          start_year),
           max_end_year = ifelse(paste0(dhs_id,"_",image_group) %in% dhs_ids_to_update$target,
                          control_props2$treat_max_end_year[i], 
                          max_end_year))

  #if we are in the last iteration for this image group, update remaining NA points (rounding
  # errors) to current iteration's values
  if (i==nrow(control_props2) |
      (i!=nrow(control_props2) & (control_props2$image_group[i] != control_props2$image_group[i+1]))) {
    dhs_c_image_group_df <- dhs_c_image_group_df %>%
      mutate(start_year = ifelse(image_group==control_props2$image_group[i] & 
                                   is.na(start_year),
                                 control_props2$treat_start_year[i], 
                                 start_year),
             max_end_year = ifelse(image_group==control_props2$image_group[i] & 
                                     is.na(max_end_year),
                                   control_props2$treat_max_end_year[i], 
                                   max_end_year))
  }  
}
# dhs_c_image_group_df %>%
#   group_by(image_group,start_year,max_end_year) %>%
#   count() %>%
#   print(n=62)

################################################################################
# Set tabular confounding variables based on start year
################################################################################
treat_count <- nrow(dhs_t_df) 
control_count <- nrow(dhs_c_image_group_df)

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
               ", control n: ",control_count,
               ", run: ",run,
               ", iterations: ",iterations
  ))
  
  ##############################################################################
  # combine treated & controls into same dataframe, join with confounders,
  # and adjust to be appropriate for start year  
  ##############################################################################
  treated_df <- dhs_t_df %>% 
    mutate(treated=1) %>% 
    select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count)
  
  control_df <- dhs_c_image_group_df %>% 
    mutate(treated=0) %>% 
    select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count)
  
  ### adjust variables to be appropriate for start_year ########################
  obs_year_df <- rbind(treated_df,control_df) %>% 
    left_join(dhs_confounders_df,by="dhs_id") %>% 
    mutate(
      iwi_est_post_oda = case_when(
        max_end_year %in% 2000:2001 ~ iwi_2002_2004_est,
        max_end_year %in% 2002:2004 ~ iwi_2005_2007_est,
        max_end_year %in% 2005:2007 ~ iwi_2008_2010_est,
        max_end_year %in% 2008:2010 ~ iwi_2011_2013_est,
        max_end_year %in% 2011:2013 ~ iwi_2014_2016_est,
        max_end_year %in% 2014:2016 ~ iwi_2017_2019_est),
      log_avg_nl_pre_oda = case_when(
        start_year %in% 2000:2001 ~ log_avg_nl_1996_1998,
        start_year %in% 2002:2004 ~ log_avg_nl_1999_2001,
        start_year %in% 2005:2007 ~ log_avg_nl_2002_2004,
        start_year %in% 2008:2010 ~ log_avg_nl_2005_2007,
        start_year %in% 2011:2013 ~ log_avg_nl_2008_2010,
        start_year %in% 2014:2016 ~ log_avg_nl_2011_2013),
      log_dist_km_to_gold = case_when(
        start_year %in% 2000:2001 ~ log_dist_km_to_gold_pre2001,
        start_year > 2001 ~ log_dist_km_to_gold_2001),
      log_dist_km_to_petro = if_else(
        start_year < 2003, 
        log_dist_km_to_petro_2000_2002,log_dist_km_to_petro_2003),
      #set dummy variables for the combination of satellite images in pre-project images
      #Landsat 5 only in images from 1990:1998 - won't include this column to avoid collinearity
      #Landsat 5&7 in images from    1999:2010 
      landsat57 = if_else(start_year %in% 2002:2013,0,1),
      #Landsat 5,7, & 8 in images from 2011:2013
      landsat578 = if_else(start_year %in% 2014:2016,0,1)
      #Landsat 7&8 in images from 2014:2019 - we aren't using any of these
      ) %>% 
    #set population density to year prior to earliest aid project
    rowwise() %>% mutate(
      log_avg_pop_dens = get(paste0("log_avg_pop_dens_",start_year - 1))
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
  input_df <- run_df %>% 
    select(dhs_id, country, iso3, lat, lon, treated, 
           start_year, image_file, iwi_est_post_oda,
           log_avg_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
           log_3yr_pre_conflict_deaths,log_trans_proj_cum_n,leader_birthplace,log_dist_km_to_gold,
           log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
           log_gdp_per_cap_USD2015,country_gini,polity2,landsat57,landsat578) %>% 
    rename(cty = country) %>% 
    mutate(cty = stringr::str_to_title(cty))
  write.csv(input_df, paste0("./data/interim/input_",run,"_",fund_sect_param,".csv"),row.names = FALSE)
  
  if (nrow(input_df[!complete.cases(input_df),]) > 0) {
    print(paste0("Stopping because incomplete cases.  See ./data/interim/input_",
                 run,"_",fund_sect_param,".csv"))
  } else {
    conf_matrix <- cbind(
      as.matrix(data.frame(
        "start_year"                 =input_df$start_year,
        "start_year_squared"         =input_df$start_year^2,
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
      )),
      model.matrix(~ cty - 1, input_df)
    )
    
    #remove any columns that have 0 standard deviation before passing to function
    before_cols <-  colnames(conf_matrix)
    conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 
    dropped_cols <- paste(var_labels[match(setdiff(before_cols, colnames(conf_matrix)),var_order)],collapse="; ")
    if (dropped_cols != "") {
      print(paste("Dropped for 0 SD: ", dropped_cols))
    }
   
    #cleanup unneeded objects in memory before calling function
    rm(control_before, control_df, control_props, control_props2, 
       country_confounders_df, dhs_c_df,dhs_c_image_group_df,dhs_confounders_df,
       dhs_ids_to_update,dhs_iso3_df,dhs_t_df,funder_sector_iso3,obs_year_df,
       run_df, treat_year_props, treated_df)
    
    ImageConfoundingAnalysis <- AnalyzeImageConfounding(
      obsW = input_df$treated,
      obsY = input_df$iwi_est_post_oda,  
      X = conf_matrix,
      long = input_df$lon,
      lat = input_df$lat,
      #concatenate the image file location and oda start year into a single keys parameter
      keys = paste0(input_df$image_file,input_df$start_year), 
      acquireImageFxn = acquireImageRepFromDisk,
      samplingType = "balancedTrain",
      nSGD = iterations,
      nDepthHidden_conv = 5L, nDepthHidden_dense = 1L, maxPoolSize = 2L, strides = 2L, kernelSize = 3L,
      nFilters = 50L,
      figuresPath = "./results/", # figures saved here
      plotBands=c(3,2,1),  #red, green, blue
      figuresTag = paste0(fund_sect_param,"_",run,"_i",iterations),
      tagInFigures = T,
      conda_env = NULL, # conda env to try to activate
      conda_env_required = F
    )
  
    ica_df <- data.frame(t(unlist(ImageConfoundingAnalysis)))
    output_df <- cbind(data.frame(run,fund_sect_param,treat_count,control_count,
                                  dropped_cols,              #t_dropped_cols,c_dropped_cols,
                                  ica_df))
    print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
                  " Writing to ./results/ICA_",fund_sect_param,"_",run,"_i"
                 ,iterations,".csv"))
    write.csv(output_df,paste0("./results/ICA_",fund_sect_param,"_",run,"_i",
                               iterations,".csv"),row.names = FALSE)
  
    ############################################################################
    # generate boxplots for this run
    ############################################################################
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
      filter(ad_sector_codes==sector_param) %>%
      pull(sec_pre_name)
    
    # Convert to long format for boxplots
    long_input_df <- input_df %>%
      select(-dhs_id, -cty, -iso3, -lat, -lon, 
             -start_year, -image_file) %>% 
      tidyr::pivot_longer(c(-treated),names_to="variable_name", values_to="value") 
  
    sub_l1 <- paste("Funder:",long_funder,"     Sector:", sector_name)
    sub_l2 <- ifelse(nzchar(dropped_cols),
                     paste0("Dropped due to no variation: ", dropped_cols),
                     "")
    
    combined_boxplot <- ggplot(long_input_df, aes(x = factor(treated), y = value)) +
      geom_boxplot() +
      labs(title = "Distribution of Wealth Outcome and Confounders for Treated and Control DHS locations",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = paste0("Treatment/Control: 0:control (n ",control_count,"), 1:treated (n ",treat_count,")"),
           y = "Value") +
      facet_wrap(~ variable_name, scales = "free") +
      facet_wrap(~ factor(variable_name, levels = var_order, labels = var_labels), scales = "free") +
      theme_bw()
    
    
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_boxplots.pdf"),
           combined_boxplot,
           width=10, height = 8, dpi=300,
           bg="white", units="in")
    
    #############################################################################
    ##### create a line plot comparing to outcome variable
    #############################################################################
    # Set the treated color based on funder
    treat_color <- case_when(
      startsWith(fund_sect_param, "ch") ~ "red",
      startsWith(fund_sect_param, "wb") ~ "blue",
      startsWith(fund_sect_param, "both") ~ "purple"
    )
    
    #Convert to longer format for density plots, leaving outcome as separate column
    hybrid_input_df <- input_df %>%
      select(-dhs_id, -cty, -iso3, -lat, -lon, 
             -start_year, -image_file) %>% 
      pivot_longer(c(-treated,-iwi_est_post_oda),names_to="variable_name", values_to="value")  
  

    outcome_confounders_plot <- ggplot(hybrid_input_df, aes(x = iwi_est_post_oda, y=value, color = factor(treated))) +
      geom_line(alpha = 0.3) +
      facet_wrap(~ variable_name, scales = "free_y", ncol = 3) +
      facet_wrap(~ factor(variable_name, levels = var_order, labels = var_labels), scales = "free") +
      labs(title = "Confounders vs. Estimated wealth for Treated and Control DHS locations",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Estimated Wealth Index 3 years post-project",
           y = "Value",
           color="Treated") +
      scale_color_manual(values = c("darkgray", treat_color),
                         labels = c("Control", "Treated")) +
      theme_bw()

    ggsave(paste0("./results/",fund_sect_param,"_",run,"_conf_iwi.pdf"),
           outcome_confounders_plot,
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
      tm_shape(input_sf[input_sf$treated == 0, ]) +  
      tm_dots(size=.3, col="gray", alpha=.3) +
      tm_shape(input_sf[input_sf$treated == 1, ]) +  
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
    
    tmap_save(treat_control_map,paste0("./results/",fund_sect_param,"_",run,"_map.png"))
    #pdf files are too large - convert png to pdf in later step
    #tmap_save(treat_control_map,paste0("./results/",fund_sect_param,"_",run,"_map.pdf")) 
    
    ############################################################################
    ##### logistical regression for treatment probabilities with tabular confounders
    ############################################################################
    conf_df <- as.data.frame(scale(conf_matrix))
    log_formula <- paste("input_df$treated ~", 
                         paste(names(conf_df), collapse = " + "))
  
    treat_prob_log <- glm(log_formula, data=conf_df, family="binomial")
  
    summary(treat_prob_log)
    #save to file for later comparison with other sectors and runs
    # saveRDS(treat_prob_log,
    #         paste0("./results/",fund_sect_param, "_", run,"_treat_prob_log.rds"))
  
    #save coeff table to dataframe, write to csv
    treat_prob_log_df <- broom::tidy(treat_prob_log)
    write.csv(treat_prob_log_df,
              paste0("./results/",fund_sect_param, "_", run,"_treat_prob_log.csv"),
              row.names = FALSE)
  
    predicted_probs <- stats::predict(treat_prob_log, type="response")
    propensity_df <- data.frame(Treatment = input_df$treated, Propensity = predicted_probs)
    
    tab_conf_density <- ggplot(propensity_df, aes(x = Propensity, fill = factor(Treatment))) +
      geom_density(alpha = 0.5) +
      labs(title = "Logistic Regression Density Plot for\nEstimated Pr(T=1 | Tabular Confounders)",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Predicted Propensity",
           y = "Density",
           fill="Status") +
      scale_fill_manual(values = c("darkgray", treat_color),
                         labels = c("Control", "Treated")) +
      theme_bw()
    
    #use this name so it will sort well in consolidated pdf
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_htreat_prop.pdf"),
           tab_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")
    
    
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
    
    ridge_coeffs <- as.matrix(coef(ridge_model_best))
    
    # Predict probabilities
    ridge_predicted_probs <- predict(ridge_model_best, newx = scale(conf_matrix),
                                     type = "response")
    
    # Create a data frame with predicted probabilities, and actual treatment status
    ridge_result_df <- data.frame(predicted_probs = ridge_predicted_probs, 
                                  treated = input_df$treated)
    
    ridge_conf_density <- ggplot(ridge_result_df, aes(x = s0, color = factor(treated))) +
      geom_density(alpha = 0.5) +
      labs(title = "Ridge regression: Density Plot for\nEstimated Pr(T=1 | Tabular Confounders)",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Predicted Propensity",
           y = "Density",
           color="Status") +
      scale_color_manual(values = c("darkgray", treat_color),
                        labels = c("Control", "Treated")) +
      theme_bw()
    
    
    #save
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_ridge_prop.pdf"),
           ridge_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")
    

    ############################################################################
    ######print these messages again to be at the end of the logfile 
    ############################################################################    
    if (dropped_cols != "") {
      print(paste("Dropped for 0 SD: ", dropped_cols))
    }
    }
}
         

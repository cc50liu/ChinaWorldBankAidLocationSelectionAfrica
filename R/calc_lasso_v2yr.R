# calc_lasso_v2yr.R
library(dplyr)
library(glmnet)
library(tidyr)     
library(ggplot2)

rm(list=ls())


#fund_sect_param <- "both_150"
# fund_sect_param <- "wb_110"
#fund_sect_param <- "ch_140"

run <- "v2yr_nonlpd"
iterations <- 1000

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")"))

#fund_sect_params <- c("both_150")
fund_sect_params <- c("both_120", "both_150", "both_210", "ch_110", "ch_120",
                      "ch_140", "ch_160", "ch_210", "ch_220", "ch_230", "ch_310",
                      "ch_320", "ch_430", "ch_520", "ch_700", "wb_110", "wb_120",
                      "wb_140", "wb_150", "wb_160", "wb_210", "wb_220", "wb_230",
                      "wb_240", "wb_310", "wb_320", "wb_330", "wb_410")

run_lasso_ridge <- function(fund_sect_param) {
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
  
  ################################################################################
  # Set tabular confounding variables based on start year
  ################################################################################
  treat_count <- nrow(dhs_t_df) 
  control_count <- nrow(dhs_c_image_group_df)
  
  print(paste0("[",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"]",
               " Processing ",fund_sect_param,
               ", treat n:",treat_count,
               ", control n: ",control_count
  ))
  
  ### combine treated & controls into same dataframe, and join with confounders ##  
  treated_df <- dhs_t_df %>% 
    mutate(treated=1) %>% 
    select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count)
  
  control_df <- dhs_c_image_group_df %>% 
    mutate(treated=0) %>% 
    select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count)
  
  obs_df <- rbind(treated_df,control_df) %>% 
    left_join(dhs_confounders_df,by="dhs_id")
  
  ### adjust variables to be appropriate for start_year ########################
  obs_year_df <- obs_df %>% 
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
  
  #create input_df 
  input_df <- run_df %>% 
    select(dhs_id, country, iso3, lat, lon, treated, 
           start_year, image_file, iwi_est_post_oda,
           log_avg_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
           log_3yr_pre_conflict_deaths,log_trans_proj_cum_n,leader_birthplace,log_dist_km_to_gold,
           log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
           log_gdp_per_cap_USD2015,country_gini,polity2,landsat57,landsat578)  
  
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
        model.matrix(~ iso3 - 1, input_df))
    
    #remove any columns that have 0 standard deviation before passing to function
    before_cols <-  colnames(conf_matrix)
    conf_matrix <- conf_matrix[,which(apply(conf_matrix,2,sd)>0)] 
    dropped_cols <- paste(var_labels[match(setdiff(before_cols, colnames(conf_matrix)),var_order)],collapse="; ")
    if (dropped_cols != "") {
      print(paste("Dropped for 0 SD: ", dropped_cols))
    }
  
    long_funder <- case_when(
      startsWith(fund_sect_param, "ch") ~ "China",
      startsWith(fund_sect_param, "wb") ~ "World Bank",
      startsWith(fund_sect_param, "both") ~ "Both China & World Bank"
    )

    sector_name <- sector_names_df %>%
      filter(ad_sector_codes==sector_param) %>%
      pull(sec_pre_name)
    
    sub_l1 <- paste("Funder:",long_funder,"     Sector:", sector_name)
    sub_l2 <- ifelse(nzchar(dropped_cols),
                     paste0("Dropped due to no variation: ", dropped_cols),
                     "")
  
    ############################################################################
    ##### ridge regression for treatment probabilities with tabular confounders
    ############################################################################
    # use cross-validation to choose lambda 
    cv_model_ridge <- cv.glmnet(x=scale(conf_matrix), y=input_df$treated,
                                family = "binomial", alpha = 0, maxit=200000)
    best_lambda_ridge <- cv_model_ridge$lambda.min
    
    # Fit model with best lambda
    ridge_model_best <- glmnet(x=scale(conf_matrix), y=input_df$treated, 
                               family = "binomial", alpha = 0, 
                               lambda = best_lambda_ridge, maxit=200000)
    
    ridge_coeffs <- as.matrix(coef(ridge_model_best))
    
    # Predict probabilities
    ridge_predicted_probs <- predict(ridge_model_best, newx = scale(conf_matrix),
                                     type = "response")
    
    # Create a data frame with actual outcomes, predicted probabilities, and treatment status
    ridge_result_df <- data.frame(predicted_probs = ridge_predicted_probs, 
                                  treated = input_df$treated)
    
    ridge_conf_density <- ggplot(ridge_result_df, aes(x = s0, color = factor(treated))) +
      geom_density(alpha = 0.5) +
      labs(title = "Ridge regression: Density Plot for\nEstimated Pr(T=1 | Tabular Confounders)",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Predicted Propensity",
           y = "Density",
           color="Status") +
      scale_color_manual(values = c("gray","black"),labels = c("Control", "Treated")) +
      theme_bw()
    
    #save
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_ridge_prop.pdf"),
           ridge_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")
    
    ############################################################################
    ##### lasso regression for treatment probabilities with tabular confounders
    ############################################################################
    # use cross-validation to choose lambda 
    cv_model_lasso <- cv.glmnet(x=scale(conf_matrix), y=input_df$treated,
                                family = "binomial", alpha = 1, maxit=200000)
    best_lambda_lasso <- cv_model_lasso$lambda.min
    
    # Fit model with best lambda
    lasso_model_best <- glmnet(x=scale(conf_matrix), y=input_df$treated,
                               family = "binomial", alpha = 1, 
                               lambda = best_lambda_lasso, maxit=200000)
    
    lasso_coeffs <- as.matrix(coef(lasso_model_best)) 
  
    # Predict probabilities
    lasso_predicted_probs <- predict(lasso_model_best, newx = scale(conf_matrix), type = "response")
    
    # Create a data frame with actual outcomes, predicted probabilities, and treatment status
    lasso_result_df <- data.frame(predicted_probs = lasso_predicted_probs, 
                                  treated = input_df$treated)
    
    lasso_conf_density <- ggplot(lasso_result_df, aes(x = s0, color = factor(treated))) +
      geom_density(alpha = 0.5) +
      labs(title = "Lasso regression: Density Plot for\nEstimated Pr(T=1 | Tabular Confounders)",
           subtitle = paste(sub_l1,sub_l2,sep="\n"),
           x = "Predicted Propensity",
           y = "Density",
           color="Status") +
      scale_color_manual(values = c("gray","black"),labels = c("Control", "Treated")) +
      theme_bw()
    
    #save
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_lasso_prop.pdf"),
           lasso_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")    
    
    return(rbind(data.frame(funder_sector=fund_sect_param, 
                            model="Ridge",
                            variable=rownames(ridge_coeffs),
                            coefs=ridge_coeffs),
                 data.frame(funder_sector = fund_sect_param,
                            model = "Lasso",
                            variable=rownames(lasso_coeffs),
                            coefs=lasso_coeffs))
    )
}


coefs_df <- do.call(rbind,lapply(fund_sect_params, run_lasso_ridge))
coefs_df %>%
  dplyr::group_by(funder_sector, model, variable) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% 
  print(n=62)

coefs_no_dups_df <- unique(coefs_df)

coefs_wide <- pivot_wider(coefs_no_dups_df, names_from = variable, values_from = s0)
write.csv(coefs_wide,paste0("./results/all_",run,"_lasso_ridge_coef.csv"),row.names=FALSE)

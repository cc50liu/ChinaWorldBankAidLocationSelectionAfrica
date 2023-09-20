# explore_tabular_conf_ongoing.R
# Compare overlap of treated and control propensities with different variables
library(dplyr)
library(tidyr)     
library(ggplot2)
library(stringr)

rm(list=ls())

################################################################################
# Initial setup, parameter processing, reading input files 
################################################################################
run <- "no_nr_dist"

dhs_confounders_df <- read.csv("./data/interim/dhs_confounders.csv") %>% 
  select(-year)  #remove survey year column that could be confused with oda year

unique_sector_funders <- read.csv("./data/interim/dhs_treat_control_ongoing_long.csv") %>% 
  filter(funder %in% c("ch","wb","both")) %>% 
  group_by(sector,funder) %>% 
  count() %>% 
  filter(n>100) %>% 
  ungroup() %>% 
  mutate(fund_sect_param=paste0(funder,"_",sector))

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")"))

################################################################################
# For loop
################################################################################
for (fund_sect_param in unique_sector_funders$fund_sect_param) {
  print(paste("processing",fund_sect_param))
  sector_param <- sub(".*_(\\d+).*", "\\1", fund_sect_param)
  funder_param <- sub("(wb|ch|both).*", "\\1", fund_sect_param)

  dhs_t_df <- read.csv("./data/interim/dhs_treat_control_ongoing_long.csv") %>% 
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
  dhs_c_df <- read.csv("./data/interim/dhs_treat_control_ongoing_long.csv") %>% 
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

  print("1")
  ################################################################################
  # Give control points same distribution of start/end years as treated points
  ################################################################################
  treat_year_props <- dhs_t_df %>%
    group_by(start_year,max_end_year) %>%
    summarize(t_start_end_n = n(), .groups = "drop") %>%
    ungroup() %>% 
    group_by(start_year) %>% 
    mutate(t_start_year_n=sum(t_start_end_n)) %>% 
    mutate(proportion = t_start_end_n / t_start_year_n) 
  
  
  control_before <- dhs_c_df %>% 
    group_by(start_year) %>% 
    count() %>% 
    rename(c_start_year_n=n)
  
  control_props <- control_before %>% 
    left_join(treat_year_props, by="start_year",multiple="all") %>% 
    mutate(desired_controls=round(c_start_year_n * proportion))
  
  #remove control rows for years with no treatments
  dhs_c_year_df <- dhs_c_df %>% 
    filter(!start_year %in% (control_props %>% 
                                filter(is.na(t_start_year_n)) %>% 
                                pull(start_year))) 
  
  control_props2 <- control_props %>% 
    filter(!is.na(t_start_year_n))
    
  print("2")
  set.seed(1234)
  for (i in 1:nrow(control_props2)) {
    #i=7  #uncomment to test
  
    if (control_props2$proportion[i] == 1) {
      #if only one max_end_year for all projects started this year, update all here
      dhs_c_year_df <- dhs_c_year_df %>%
        mutate(max_end_year = ifelse(start_year==control_props2$start_year[i],
                                     control_props2$max_end_year[i],
                                     max_end_year))
      
    } else {
      #we have multiple max_end_years for projects started this year
      #if we are in the last iteration for this year, update remaining NA points (rounding
      # errors) to current iteration's values
      if (i==nrow(control_props2) |
          (i!=nrow(control_props2) 
           & (control_props2$start_year[i] != control_props2$start_year[i+1]))) {
        dhs_c_year_df <- dhs_c_year_df %>%
          mutate(max_end_year = ifelse(start_year==control_props2$start_year[i] & 
                                         is.na(max_end_year),
                                       control_props2$max_end_year[i],
                                       max_end_year))
      } else {
        # multiple max_end_years to adjust for this start year, randomly
        #select control points to be assigned the end year on this row
        dhs_ids_to_update <- dhs_c_year_df %>%
          filter(start_year==control_props2$start_year[i] & 
                   is.na(max_end_year)) %>% 
          slice_sample(n=control_props2$desired_controls[i], replace = FALSE) %>% 
          select(dhs_id, start_year) %>% 
          mutate(target=paste0(dhs_id,"_",start_year))
        
        # Update randomly selected control dhs_ids with year of this loop
        dhs_c_year_df <- dhs_c_year_df %>%
          mutate(max_end_year = ifelse(paste0(dhs_id,"_",start_year) %in% dhs_ids_to_update$target, 
                                       control_props2$max_end_year[i], 
                                       max_end_year))
      }
    }  
  }
  print("3")

  treat_count <- nrow(dhs_t_df) 
  control_count <- nrow(dhs_c_year_df)

  ##############################################################################
  # combine treated & controls into same dataframe, join with confounders,
  # and adjust to be appropriate for start year  
  ##############################################################################
  obs_year_df <- rbind(
    dhs_t_df %>% 
       mutate(treated=1) %>% 
       select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count),
    dhs_c_year_df %>% 
       mutate(treated=0) %>% 
       select(dhs_id,start_year,treated,max_end_year,end_year_imputed,proj_count)
    ) %>% 
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
  print("4")  
  #join to country-level parameters, which are year specific
  country_confounders_df <- read.csv("./data/interim/country_confounders.csv") %>% 
    select(-country) %>% 
    mutate(year=year+1)  #add 1 to year for join below, to get 1 year pre-project

  print("5")
  run_df <- obs_year_df %>% 
    left_join(country_confounders_df,
              by=join_by(iso3, start_year == year))
  print("6")
  #create input_df 
  input_df <- run_df %>% 
    select(dhs_id, country, iso3, lat, lon, treated, 
           start_year, image_file, iwi_est_post_oda,
           log_avg_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
           log_3yr_pre_conflict_deaths,log_trans_proj_cum_n,leader_birthplace,log_dist_km_to_gold,
           log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
           log_gdp_per_cap_USD2015,country_gini,polity2,landsat57,landsat578) %>% 
    rename(cty = country) %>% 
    mutate(cty = stringr::str_to_title(cty))
  
  print("after created input_df")
  print(paste("nrow(input_df[!complete.cases(input_df),])",
              nrow(input_df[!complete.cases(input_df),])))
  if (nrow(input_df[!complete.cases(input_df),]) > 0) {
    print(paste0("Stopping because incomplete cases.  See ./data/interim/input_",
                 run,"_",fund_sect_param,".csv"))
  } else {
    conf_matrix <- cbind(
      as.matrix(data.frame(
        "start_year"                 =input_df$start_year,
        "start_year_squared"         =as.integer(input_df$start_year)*as.integer(input_df$start_year),
        "log_avg_nl_pre_oda"         =input_df$log_avg_nl_pre_oda,          #scene level
        "log_avg_min_to_city"        =input_df$log_avg_min_to_city,         #scene level
        "log_avg_pop_dens"           =input_df$log_avg_pop_dens,            #scene level
        "leader_birthplace"          =input_df$leader_birthplace           #inherited from ADM1
        #"log_dist_km_to_gold"        =input_df$log_dist_km_to_gold,         #scene level
        #"log_dist_km_to_gems"        =input_df$log_dist_km_to_gems,         #scene level
        #"log_dist_km_to_dia"         =input_df$log_dist_km_to_dia,          #scene level
        #"log_dist_km_to_petro"       =input_df$log_dist_km_to_petro         #scene level
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

    ############################################################################
    # setup variables for plots
    ############################################################################
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
    
    # Set the treated color based on funder
    treat_color <- case_when(
      startsWith(fund_sect_param, "ch") ~ "indianred1",
      startsWith(fund_sect_param, "wb") ~ "lightblue1",
      startsWith(fund_sect_param, "both") ~ "blueviolet"
    )

    ############################################################################
    ##### logistical regression for treatment probabilities with tabular confounders
    ############################################################################
    conf_df <- as.data.frame(scale(conf_matrix))
    log_formula <- paste("input_df$treated ~", 
                         paste(names(conf_df), collapse = " + "))
  
    treat_prob_log <- glm(log_formula, data=conf_df, family="binomial")
  
    #save coeff table to dataframe, write to csv
    treat_prob_log_df <- broom::tidy(treat_prob_log)
    write.csv(treat_prob_log_df,
              paste0("./results/",fund_sect_param, "_", run,"_treat_prob_log.chttp://127.0.0.1:35635/graphics/plot_zoom_png?width=858&height=900sv"),
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
      scale_fill_manual(values = c("gray60", treat_color),
                         labels = c("Control", "Treated")) +
      theme_bw() 
    
    #use this name so it will sort well in consolidated pdf
    ggsave(paste0("./results/",fund_sect_param,"_",run,"_htreat_prop.pdf"),
           tab_conf_density,
           width=6, height = 4, dpi=300,
           bg="white", units="in")
    
  }
}
      
   





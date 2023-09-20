# explore_tabular_conf_ongoing_nodist_pcnl.R
# Compare overlap of treated and control propensities with different variables
library(dplyr)
library(tidyr)     
library(ggplot2)
library(stringr)

rm(list=ls())

################################################################################
# Initial setup, parameter processing, reading input files 
################################################################################
run <- "no_nr_dist_pcnl"

dhs_confounders_df <- read.csv("./data/interim/dhs_confounders.csv") %>% 
  select(-year)  #remove survey year column that could be confused with oda year

unique_sector_funders <- read.csv("./data/interim/dhs_treat_control_collaps_end_dates.csv") %>% 
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

  dhs_t_df <- read.csv("./data/interim/dhs_treat_control_collaps_end_dates.csv") %>% 
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
  dhs_c_df <- read.csv("./data/interim/dhs_treat_control_collaps_end_dates.csv") %>% 
    filter(sector==sector_param & funder=="control") %>% 
    inner_join(dhs_iso3_df,by="dhs_id") %>% 
    filter(iso3 %in% funder_sector_iso3$iso3 & 
           dhs_id %in% dhs_confounders_df$dhs_id) 
           #exclude 5 DHS points where confounder data not available )
    

 #define variable order and names for boxplots and dropped cols variables
  var_order_all <- c("iwi_est_post_oda","log_pc_nl_pre_oda","log_avg_pop_dens",
                 "log_avg_min_to_city",
                 "log_dist_km_to_gold","log_dist_km_to_gems",        
                 "log_dist_km_to_dia","log_dist_km_to_petro", 
                 "leader_birthplace","log_trans_proj_cum_n",
                 "log_3yr_pre_conflict_deaths",
                 "landsat57",
                 "landsat578")
  var_labels_all <- c("Wealth (est, t+3)","Nightlights per capita (t-1,log)","Pop Density (t-1,log)",
                  "Minutes to City (2000,log)","Dist to Gold (km,log)",
                  "Dist to Gems (km,log)","Dist to Diam (km,log)",
                  "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                  "Conflict deaths (t-1,log)", 
                  "Landsat 5 & 7", "Landsat 5,7,& 8")
  print("1")
  ################################################################################
  # Give control points same distribution of start/end years as treated points
  ################################################################################
  year_props <- dhs_t_df %>%
    #if end year>2016, consider it 2016 here for selection of IWI years
    mutate(max_end_year = ifelse(max_end_year>2016,2016,max_end_year)) %>% 
    group_by(min_start_year,max_end_year) %>%
    summarize(count = n(), .groups = "drop") %>%
    ungroup() %>% 
    mutate(proportion = count / nrow(dhs_t_df)) %>% 
    mutate(cntl_count = round(nrow(dhs_c_df) * proportion))   


  set.seed(1234)
  for (i in 1:nrow(year_props)) {
    #i=1  #uncomment to test
    
    # Randomly select control points to be assigned loop's current year
    dhs_ids_to_update <- dhs_c_df %>%
      filter(is.na(min_start_year)) %>% 
      slice_sample(n=year_props$cntl_count[i], replace = FALSE) %>% 
      pull(dhs_id)
    
    # Update randomly selected control dhs_ids with year of this loop
    dhs_c_df <- dhs_c_df %>%
      mutate(min_start_year = ifelse(dhs_id %in% dhs_ids_to_update,
                                     year_props$min_start_year[i],
                                     min_start_year),
             max_end_year = ifelse(dhs_id %in% dhs_ids_to_update,
                                   year_props$max_end_year[i], 
                                   max_end_year))
    
    #if we are in the last iteration, update remaining NA points (due to rounding
    # errors) to this year
    if (i==nrow(year_props)) {
      dhs_c_df <- dhs_c_df %>%
        mutate(min_start_year = ifelse(is.na(min_start_year),
                                       year_props$min_start_year[i],
                                       min_start_year),
               max_end_year = ifelse(is.na(max_end_year),
                                     year_props$max_end_year[i], 
                                     max_end_year))
    }
  }

  print("2")

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
                 " Call AnalyzeImageConfounding for ",fund_sect_param,
                 ", treat n:",treat_count,
                 ", control n: ",control_count,
                 ", run: ",run
    ))
    

    ##############################################################################
    # combine treated & controls into same dataframe, join with confounders,
    # and adjust to be appropriate for start year  
    ##############################################################################
    ##############################################################################
    # combine treated & controls into same dataframe, join with confounders,
    # and adjust to be appropriate for start year  
    ##############################################################################
    obs_df <- rbind(
      dhs_t_df %>% 
        mutate(treated=1) %>% 
        select(dhs_id,min_start_year,treated,max_end_year,proj_count),
      dhs_c_df %>% 
        mutate(treated=0) %>% 
        select(dhs_id,min_start_year,treated,max_end_year,proj_count)
    ) %>% 
      left_join(dhs_confounders_df,by="dhs_id") %>% 
      mutate(
        iwi_est_post_oda = case_when(
          max_end_year %in% 2000:2001 ~ iwi_2002_2004_est,
          max_end_year %in% 2002:2004 ~ iwi_2005_2007_est,
          max_end_year %in% 2005:2007 ~ iwi_2008_2010_est,
          max_end_year %in% 2008:2010 ~ iwi_2011_2013_est,
          max_end_year %in% 2011:2013 ~ iwi_2014_2016_est,
          max_end_year >= 2014 ~ iwi_2017_2019_est),
        log_dist_km_to_gold = case_when(
          min_start_year %in% 2000:2001 ~ log_dist_km_to_gold_pre2001,
          min_start_year > 2001 ~ log_dist_km_to_gold_2001),
        log_dist_km_to_petro = if_else(
          min_start_year < 2003, 
          log_dist_km_to_petro_2000_2002,log_dist_km_to_petro_2003),
        #set dummy variables for the combination of satellite images in pre-project images
        #Landsat 5 only in images from 1990:1998 - won't include this column to avoid collinearity
        #Landsat 5&7 in images from    1999:2010 
        landsat57 = if_else(min_start_year %in% 2002:2013,1,0),
        #Landsat 5,7, & 8 in images from 2011:2013
        landsat578 = if_else(min_start_year %in% 2014:2016,1,0)
        #Landsat 7&8 in images from 2014:2019 - we aren't using any of these
      ) %>% 
      #set per cap nl to year prior to earliest aid project
      rowwise() %>% mutate(
        log_pc_nl_pre_oda = get(paste0("log_pc_nl_",min_start_year - 1))
      ) %>% ungroup() %>%
      #set population density to year prior to earliest aid project
      rowwise() %>% mutate(
        log_avg_pop_dens = get(paste0("log_avg_pop_dens_",min_start_year - 1))
      ) %>% ungroup() %>% 
      #set leader_birthplace based on year prior to earliest aid project 
      rowwise() %>% mutate(
        leader_birthplace = get(paste0("leader_", min_start_year - 1))
      ) %>% ungroup() %>% 
      #set conflict deaths to 3 year pre-project sum 
      rowwise() %>%  mutate(
        log_3yr_pre_conflict_deaths = get(paste0("log_deaths", 
                                                 min_start_year - 3,
                                                 "_",
                                                 min_start_year - 1))
      ) %>% ungroup() %>%  
      #set loan-based transport projects based on year prior to earliest aid project 
      rowwise() %>% mutate(
        log_trans_proj_cum_n = get(paste0("log_trans_proj_cum_n_",
                                          min_start_year - 1))
      ) %>% ungroup()  
  
    print("3")
    #create input_df 
    input_df <- obs_df %>% 
      select(dhs_id, country, iso3, lat, lon, treated, 
             min_start_year, image_file, iwi_est_post_oda,
             log_pc_nl_pre_oda,log_avg_min_to_city,log_avg_pop_dens,
             log_3yr_pre_conflict_deaths,log_trans_proj_cum_n,leader_birthplace,log_dist_km_to_gold,
             log_dist_km_to_gems,log_dist_km_to_dia,log_dist_km_to_petro,
             landsat57,landsat578) %>% 
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
          "log_pc_nl_pre_oda"          =input_df$log_pc_nl_pre_oda,           #scene level
          "log_avg_min_to_city"        =input_df$log_avg_min_to_city,         #scene level
          "log_avg_pop_dens"           =input_df$log_avg_pop_dens,            #scene level
          "log_3yr_pre_conflict_deaths"=input_df$log_3yr_pre_conflict_deaths, #inherited from ADM1
          "leader_birthplace"          =input_df$leader_birthplace,           #inherited from ADM1
          "log_trans_proj_cum_n"       =input_df$log_trans_proj_cum_n,        #inherited from ADM1, ADM2
          "landsat57"                  =input_df$landsat57,                   #pre-treat image
          "landsat578"                 =input_df$landsat578                   #pre-treat image 
        )),
        model.matrix(~ cty - 1, input_df)
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
      sub_l2 <- ifelse(nzchar(dropped_labels),
                       paste0("Dropped due to no variation: ", dropped_labels),
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
}
      
   





#consolidate_CI_output_v2.R
# consolidate .csv files produced by AnalyzeCausalImages runs (after 8/24)
#   into a single csv
# parameter:  run version ("v9","v10",etc)
library(data.table)

rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
run_version <- args[1]
#uncomment to test
run_version <-"pcnl_sh_co_d1"
run_directory <- "./results/test_runs/pcnl_sh_co_d1/"

#filename_pattern <- paste0("ICA_",run_version,"_(wb|ch|both)_\\d{3}.csv")
filename_pattern <- paste0("ICA_(wb|ch|both)_\\d{3}_",run_version,".*.csv")

# Get the list files from the run in the directory
matching_files <- list.files(run_directory, pattern = filename_pattern,
                             full.names = TRUE)

# function to read files and exclude cols with individual point probabilities
read_and_process_file <- function(file) {
  data <- fread(file, header = TRUE, drop = grep("^prW_est", colnames(fread(file))))
  return(data)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_df <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)



write.csv(consolidated_df,paste0(run_directory,"/ICA_",run_version,"_all.csv"),
                               row.names=FALSE)
#consolidated_df <- read.csv(paste0("./ICA_",run_version,"_all.csv"))
          
library(dplyr)
library(tidyr)
outcome_df <- consolidated_df %>% 
  select(fund_sect_param, treat_count,control_count, 
         tauHat_propensityHajek, tauHat_propensityHajek_se) %>% 
  rename(fund_sec = fund_sect_param,
         treat_n = treat_count,
         control_n = control_count,
         tauHat_prop = tauHat_propensityHajek,
         tauHat_prop_se = tauHat_propensityHajek_se
         ) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec),
         sig = ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.96, "***",
               ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.645, "**", 
               ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.282, "*", ""))),
         ate = paste0(round(tauHat_prop,2)," (",
                             round(tauHat_prop_se,2),")",sig)
         ) %>%
  arrange(sector)

  
sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, sec_pre_name)

outcome_sector_df <- outcome_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(sec_pre_name, funder, ate) %>% 
  pivot_wider(names_from=funder, values_from=ate) %>% 
  rename(Both=both,
         China=ch,
         World_Bank=wb) 

write.table(outcome_sector_df,paste0(run_directory,"/outcome_",run_version,".csv"),
          sep=";",dec=".",row.names=FALSE)

var_order <- c("log_avg_nl_pre_oda","log_avg_pop_dens",
               "log_avg_min_to_city",
               "log_dist_km_to_gold","log_dist_km_to_gems",
               "log_dist_km_to_dia","log_dist_km_to_petro",
               "leader_birthplace","log_trans_proj_cum_n",
               "log_3yr_pre_conflict_deaths",
               "polity2","log_gdp_per_cap_USD2015","country_gini","landsat57",
               "landsat578")
var_labels <- c("Nightlights (t-3,log)","Pop Density (t-1,log)",
                "Minutes to City (2000,log)","Dist to Gold (km,log)",
                "Dist to Gems (km,log)","Dist to Diam (km,log)",
                "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                "Conflict deaths (t-1,log)",
                "Country Polity2 (t-1)","Cntry GDP/cap (t-1,log)","Country gini (t-1)",
                "Landsat 5 & 7", "Landsat 5,7,& 8")


#tabular confounding variables by sector
tab_conf_salience_df <- consolidated_df %>% 
  select(fund_sect_param, starts_with("SalienceX."))  %>% 
  rename_with(~sub("^SalienceX\\.", "", .), starts_with("SalienceX.")) %>% 
  rename_with(~var_labels[var_order == .x], .cols = all_of(var_order)) %>% 
  rename(fund_sec = fund_sect_param) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec))   


tab_conf_sal_sector_df <- tab_conf_salience_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(sec_pre_name, sector, funder, all_of(var_labels)) %>% 
  mutate(across(all_of(var_labels),~round(.,2))) %>% 
  arrange(sector, funder) %>% 
  mutate(funder = case_match(funder,"both" ~ "Both",
                           "ch" ~ "CH",
                           "wb" ~ "WB")) %>% 
  select(-sector) %>% 
  rename(Funder=funder)

write.table(tab_conf_sal_sector_df,paste0(run_directory,"/outcome_tab_conf_salience_",run_version,".csv"),
            sep=";",dec=".",row.names=FALSE)

tab_conf_sal_sector_df %>% 
  filter(Funder=="Both") %>% 
  select(-Funder) %>% 
  write.table(paste0(run_directory,"/outcome_both_tab_conf_salience_",run_version,".csv"),
           sep=";",dec=".",row.names=FALSE)

tab_conf_sal_sector_df %>% 
  filter(Funder=="China") %>% 
  select(-Funder) %>% 
  write.table(paste0(run_directory,"/outcome_ch_tab_conf_salience_",run_version,".csv"),
              sep=";",dec=".",row.names=FALSE)

tab_conf_sal_sector_df %>% 
  filter(Funder=="World_Bank") %>% 
  select(-Funder) %>% 
write.table(paste0(run_directory,"/outcome_wb_tab_conf_salience_",run_version,".csv"),
           sep=";",dec=".",row.names=FALSE)

#process logistic regressions of tabular confounders only, adding a column for salience from image processing
log_reg_pattern <- paste0(".*",run_version,"_treat_prob_log.csv")

# Get the list files from the run in the directory
logistic_files <- list.files(run_directory, pattern = log_reg_pattern,
                             full.names = TRUE)

output <- lapply(logistic_files, function(lfile) {
  #uncomment to test
  #lfile <- "./results/no_trans_death/ch_140_no_trans_death_treat_prob_log.csv"
  #extract funder/sector from filename
  fund <- sub(".*/(wb|ch|both)_\\d{3}.*", "\\1", lfile)
  sec <- sub(".*/(wb|ch|both)_(\\d{3}).*","\\2",lfile)
  fund_sec <- paste0(fund,"_",sec)
  
  treat_prob_log_df <- read.csv(lfile)
  
  fund_sect_images_df <- consolidated_df %>% 
    select(fund_sect_param, starts_with("SalienceX."))  %>% 
    rename_with(~sub("^SalienceX\\.", "", .), starts_with("SalienceX.")) %>% 
    filter(fund_sect_param==fund_sec) %>% 
    pivot_longer(cols=-fund_sect_param,names_to = "term", values_to = "image_estimate")
  
  treat_prob_log_images_df <- treat_prob_log_df %>% 
    left_join(fund_sect_images_df,by="term") %>% 
    select(term, image_estimate, estimate, std.error, statistic, p.value,fund_sect_param) %>% 
    rename(logistic_estimate=estimate)
  
  write.csv(treat_prob_log_images_df,
            paste0(run_directory,"/",fund_sec,"_images_log_treat_prob.csv"),
            row.names = FALSE)
})

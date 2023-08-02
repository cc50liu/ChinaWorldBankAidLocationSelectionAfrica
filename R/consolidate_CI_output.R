#consolidate_CI_output.R
# consolidate .csv files produced by AnalyzeCausalImages calls into a single csv
# parameter:  run version ("v9","v10",etc)
library(data.table)

rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
run_version <- args[1]
#uncomment to test
#run_version <-"v14"

#filename_pattern <- paste0("ICA_",run_version,"_(wb|ch|both)_\\d{3}.csv")
filename_pattern <- paste0("ICA_(wb|ch|both)_\\d{3}_",run_version,".csv")

# Get the list files from the run in the directory
matching_files <- list.files("./results/", pattern = filename_pattern,
                             full.names = TRUE)

# function to read files and exclude cols with individual point probabilities
read_and_process_file <- function(file) {
  data <- fread(file, header = TRUE, drop = grep("^prWEst", colnames(fread(file))))
  return(data)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_df <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)

write.csv(consolidated_df,paste0("./results/ICA_",run_version,"_all.csv"),
                                 row.names=FALSE)
library(dplyr)
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
         treat_prob = paste0(round(tauHat_prop,2)," (",
                             round(tauHat_prop_se,2),")",sig)
         ) %>%
  arrange(sector)

  
sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, sec_pre_name)

outcome_sector_df <- outcome_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(sec_pre_name, funder, treat_prob) %>% 
  pivot_wider(names_from=funder, values_from=treat_prob) %>% 
  rename(Both=both,
         China=ch,
         World_Bank=wb)


outcome_sector_df

write.csv(outcome_sector_df,paste0("./results/outcome_",run_version,".csv"),
          row.names=FALSE)


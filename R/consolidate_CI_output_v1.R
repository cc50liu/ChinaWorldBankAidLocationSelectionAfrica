#consolidate_CI_output.R
# consolidate .csv files produced by AnalyzeCausalImages calls into a single csv
library(dplyr)

rm(list=ls())
#change this pattern for different run versions
run_prefix <- "ICA_5b_2000i_dhs_ADM2_"
filename_pattern <- paste0(run_prefix,"\\d{3}_(wb|ch|both)_\\d{3}.csv")

# Get the list of files matching the pattern in the directory
matching_files <- list.files("./data/interim/", pattern = filename_pattern,
                             full.names = TRUE)

# Loop through the matching files
for (current_file in matching_files) {
  current_df <- read.csv(current_file) %>% 
    #modify this as the file structure changes; I'm excluding rownames and 
    #treatment probabilities for individual points
    select(2:14)
  if (!exists("consolidated_df")) {
    consolidated_df <- current_df
  } else {
    consolidated_df <- rbind(consolidated_df, current_df)
  }
}

write.csv(consolidated_df,paste0("./data/interim/",run_prefix,"consol.csv"),
                                 row.names=FALSE)



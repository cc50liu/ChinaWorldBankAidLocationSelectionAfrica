#consolidate_CI_output.R
# consolidate .csv files produced by AnalyzeCausalImages calls into a single csv
# parameter:  run version ("v9","v10",etc)
library(data.table)

rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
run_version <- args[1]
#uncomment to test
run_version <-"v10"

filename_pattern <- paste0("ICA_",run_version,"_(wb|ch|both)_\\d{3}.csv")

# Get the list files from the run in the directory
matching_files <- list.files("./data/interim/", pattern = filename_pattern,
                             full.names = TRUE)

# function to read files and exclude cols with individual point probabilities
read_and_process_file <- function(file) {
  data <- fread(file, header = TRUE, drop = grep("^prWEst", colnames(fread(file))))
  return(data)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_df <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)

write.csv(consolidated_df,paste0("./data/interim/ICA_",run_version,"_all.csv"),
                                 row.names=FALSE)

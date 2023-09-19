#consolidate_CI_output_v3.R
# consolidate .csv files produced by AnalyzeCausalImages runs (after 8/24)
#   into a single csv
# parameter:  run version ("v9","v10",etc)
library(dplyr)
library(data.table)
library(ggplot2)

rm(list=ls())
args <- commandArgs(trailingOnly = TRUE)
run_version <- args[1]
#uncomment to test
run_version <-"tfrec_emb_boot30"
run_directory <- "./results/tfrec_emb_boot30/"

subdirectories <- c("BasicServices","Infrastructure","Interventions","Other")

################################################################################
#function to consolidate sector/funders into a file for each subdirectory group
# and produce files with estimated average treatment effects
#
#uses data.table since it handles missing columns
################################################################################
process_subdir <-  function(run_dir,sub_dir) {
  #uncomment to test
  #run_dir <- run_directory
  #sub_dir <- "BasicServices"
  
  full_dir <- paste0(run_dir,sub_dir,"/")
  #filename_pattern <- paste0("ICA_",run_version,"_(wb|ch|both)_\\d{3}.csv")
  filename_pattern <- paste0("ICA_(wb|ch|both)_\\d{3}_",run_version,".*.csv")
  
  # Get the list files from the run in the directory
  matching_files <- list.files(full_dir, pattern = filename_pattern,
                               full.names = TRUE)
  
  # function to read files and exclude cols with individual point probabilities
  read_and_process_file <- function(file) {
    data <- fread(file, header = TRUE, drop = grep("^prW_est", colnames(fread(file))))
    return(data)
  }
  
  # Read and merge the files, filling columns that don't exist in some with NAs
  consolidated_dt <- rbindlist(lapply(matching_files, read_and_process_file), 
                               use.names = TRUE, fill = TRUE)
  
  write.csv(consolidated_dt,paste0(run_dir,"/ICA_",sub_dir,"_",run_version,"_all.csv"),
                                 row.names=FALSE)
  #consolidated_dt <- read.csv(paste0("./ICA_",run_version,"_all.csv"))

                                #select and rename columns
  outcome_dt <- consolidated_dt[, .(fund_sec = fund_sect_param,
                                    treat_n = treat_count,
                                    control_n = control_count,
                                    tauHat_prop = tauHat_propensityHajek,
                                    tauHat_prop_se = tauHat_propensityHajek_se)][,
                #chain command to calculate new columns                                                
                `:=`(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
                     funder = sub("(wb|ch|both).*", "\\1", fund_sec),
                     sig = ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.96, "***",
                           ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.645, "**", 
                           ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.282, "*", ""))))]
  
  #use sig calculated above in ate
  outcome_dt  <- outcome_dt[,ate := paste0(round(tauHat_prop, 2), " (", round(tauHat_prop_se, 2), ")", sig)][, 
             #chain command to select columns and order rows                                                                                     
      .(fund_sec, treat_n, control_n, tauHat_prop, tauHat_prop_se, sector, funder, sig, ate)]
  #order by sector
  outcome_dt  <- setorder(outcome_dt, sector)

  # Read sector_group_names.csv and convert to data.table
  sector_names_dt <- fread("./data/interim/sector_group_names.csv")
  sector_names_dt <- sector_names_dt[, sec_pre_name := paste0(ad_sector_names, " (", ad_sector_codes, ")")][
    #chain command to select columns
    , .(ad_sector_codes, sec_pre_name)]
  
  # Perform inner join  
  outcome_sector_dt <- outcome_dt[sector_names_dt, on=.(sector = ad_sector_codes), nomatch=NULL]
  
  # Format tabular data for display
  outcome_sector_display_dt <- outcome_sector_dt[, .(sec_pre_name, funder, ate)]
  #reshape to have columns by funder
  sector_funder_dt <- dcast(outcome_sector_display_dt, sec_pre_name ~ funder, value.var = "ate")
  setnames(sector_funder_dt, c("both", "ch", "wb"), c("Both", "China", "World_Bank"), skip_absent=TRUE)
  
  # Write to csv file
  write.table(sector_funder_dt, paste0(run_directory, "/outcome_display",sub_dir,"_",run_version, ".csv"), sep = ";", dec = ".", row.names = FALSE)  

  #generate figure showing ate by sector funder  
  ate_plot <- ggplot(outcome_sector_dt,aes(x=tauHat_prop,y=sec_pre_name,color=funder)) +
    geom_pointrange(aes(xmin=tauHat_prop-(tauHat_prop_se*1.96),
                        xmax=tauHat_prop+(tauHat_prop_se*1.96)),
                    position = position_jitter(height=0.2)) +
    geom_vline(xintercept=0,color="black") +
    scale_color_manual(values = c("ch" = "tomato3", "wb" = "steelblue1", "both" = "purple"),
                       labels = c("ch" = "China","wb"="World Bank","both"="Both")) +  
    labs(title = "Average Treatment Effect (est) on Wealth by Funder and Sector",
         subtitle = paste0("Sectors:  ",
                           ifelse(sub_dir=="BasicServices","Basic Services",sub_dir),
                           "     Run: ",run_version),
         x = "Estimated ATE with 95% confidence intervals",
         y = "",
         color="Funder") +
    theme_bw()
  
  ggsave(paste0(run_directory,"/",sub_dir,"_",run_version,"_ate_funder_sector.pdf"),
         ate_plot,
         width=10, height = 6, dpi=300,
         bg="white", units="in")
}       

################################################################################
#call the function
################################################################################
output <- sapply(subdirectories,process_subdir, run_dir=run_directory)
  




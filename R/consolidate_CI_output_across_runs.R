#consolidate_CI_output_across_runs.R
# consolidate .csv files produced by AnalyzeCausalImages runs (after 9/9)
#   into a single csv
# v4 makes graphs for Latitude Analysis and ModelEvaluationMetrics
# parameter:  run version ("v9","v10",etc)
library(dplyr)
#use data.table since it handles missing columns
library(data.table)
library(ggplot2)

rm(list=ls())

#uncomment to test
run_versions <- c("tfrec_cnn_agglom","tfrec_emb_agglom")
run_directories <-c("./results/tfrec_cnn_agglom",
                    "./results/tfrec_emb_agglom")
run_shortnames <- c("cnn","emb")
group_label_for_filename <- "cnn_emb_aggl"

#function to get matching files for a specific run
get_matching_files <- function(run_version, run_directory) {
  #uncomment to test
  # run_version <- "tfrec_cnn_agglom"
  # run_directory <- "./results/tfrec_cnn_agglom"
  
  #filename_pattern <- paste0("ICA_",run_version,"_(wb|ch|both)_\\d{3}.csv")
  filename_pattern <- paste0("ICA_(wb|ch|both)_\\d{3}_",run_version,".*.csv")
  
  # Get the list files from the run in the directory
  matching_files <-  list.files(run_directory, pattern = filename_pattern,
                               full.names = TRUE)
  return(matching_files)
}
  
# Apply the function to the runs we're comparing
output <- mapply(get_matching_files, run_versions, run_directories)
  
matching_files <- unlist(output)

# function to read files and exclude cols with individual point probabilities
read_and_process_file <- function(file) {
  data <- fread(file, header = TRUE, drop = grep("^prW_est", colnames(fread(file))))
  return(data)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_dt <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)

write.csv(consolidated_dt,paste0("./results/ICA_xruns_",group_label_for_filename,".csv"),
          row.names=FALSE)
#consolidated_dt <- read.csv(paste0(run_dir,"/ICA_",sub_dir,"_",run_version,"_all.csv"))

library(tidyr)
outcome_df <- consolidated_dt %>% 
  select(run, fund_sect_param, 
         tauHat_propensityHajek, tauHat_propensityHajek_se) %>% 
  rename(fund_sec = fund_sect_param,
         tauHat_prop = tauHat_propensityHajek,
         tauHat_prop_se = tauHat_propensityHajek_se
  ) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec),
         sig = ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.96, "***",
                      ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.645, "**", 
                             ifelse(abs(tauHat_prop) / tauHat_prop_se >= 1.282, "*", ""))),
         ate = paste0(round(tauHat_prop,2)," (",
                      round(tauHat_prop_se,2),")",sig),
         run_short = run_shortnames[match(run,run_versions)]
  ) %>%
  arrange(sector)  

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, sec_pre_name)

outcome_sector_df <- outcome_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) 

outcome_sector_display_df <- outcome_sector_df %>% 
  select(sec_pre_name, run_short, funder, ate) %>% 
  pivot_wider(names_from=funder, values_from=ate) %>% 
  rename(Both=both,
         China=ch,
         World_Bank=wb,
         Run=run_short,
         Sector=sec_pre_name
         ) 


# Write to csv file
write.csv(outcome_sector_display_df,paste0("./results/outcome_display_xruns_",group_label_for_filename,".csv"),
          row.names=FALSE)


#generate figure showing ate by sector funder and run 
ate_plot <- ggplot(outcome_sector_df,aes(x=tauHat_prop,y=sec_pre_name,color=funder,shape=run_short)) +
  geom_pointrange(aes(xmin=tauHat_prop-(tauHat_prop_se*1.96),
                      xmax=tauHat_prop+(tauHat_prop_se*1.96)),
                  position = position_jitter(height=0.2)) +
  geom_vline(xintercept=0,color="black") +
  scale_color_manual(values = c("ch" = "red", "wb" = "blue", "both" = "purple"),
                     labels = c("ch" = "China","wb"="World Bank","both"="Both")) +  
  labs(title = "Average Treatment Effect (est) on Wealth by Funder and Sector",
       x = "Estimated ATE with 95% confidence intervals",
       y = "",
       color="Funder",
       shape="Run") +
  theme_bw()  +
  theme(panel.grid = element_blank())

ggsave(paste0("./results/ate_funder_sector_xruns_",group_label_for_filename,".pdf"),
       ate_plot,
       width=10, height = 6, dpi=300,
       bg="white", units="in")


##############################################################################
# latitude differences, pre and post IPW, between treatment and control groups
##############################################################################
#create sector and funder columns, order by sector
consolidated_df <- consolidated_dt %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sect_param)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sect_param),
         run_short = run_shortnames[match(run,run_versions)],
         fund_sec_run = paste0(fund_sect_param,"_",run_short)
         ) %>% 
  select(-starts_with("SGD_loss")) %>% 
  arrange(sector) 

line_color <- ifelse(abs(consolidated_df$LatitudeAnalysis.preDiffInLat1) > 
                       abs(consolidated_df$LatitudeAnalysis.postDiffInLat1),
                     "lightgray", "black")

difInLatPlot <- ggplot(consolidated_df, aes(x = fund_sec_run)) +
  geom_segment(aes(x=reorder(fund_sec_run,sector),
                   xend=fund_sec_run,
                   y=LatitudeAnalysis.preDiffInLat1,
                   yend=LatitudeAnalysis.postDiffInLat1),
               linewidth = 1,
               color = line_color, 
               show.legend=FALSE) +
  geom_point(aes(y = LatitudeAnalysis.preDiffInLat1, shape = "Pre", color=funder), size = 2) +
  geom_point(aes(y = LatitudeAnalysis.postDiffInLat1, shape = "Post",color=funder), size = 2) +
  geom_hline(yintercept=0, color="black") +
  scale_shape_manual(values = c("Pre" = 16, "Post" = 15),
                     breaks = c("Pre","Post")) +
  scale_color_manual(values = c("ch" = "red", "wb" = "blue", "both" = "purple"),
                     labels = c("China", "World Bank", "Both")) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Average Difference in Treated and Control Latitudes",
    shape = "IPW Adjustment",
    color = "Funder",
    title = "Pre- and Post- Inverse Probability Weighting",
    subtitle = "Average Difference in Treated and Control Latitudes") +
  theme_bw() +  
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/difInLat_xruns_",group_label_for_filename,".pdf"),
       difInLatPlot,
       width=8, height = 6, dpi=300,
       bg="white", units="in")

##############################################################################
# longitude differences, pre and post IPW, between treatment and control groups
##############################################################################  
line_color <- ifelse(abs(consolidated_df$LatitudeAnalysis.preDiffInLat2) > 
                       abs(consolidated_df$LatitudeAnalysis.postDiffInLat2),
                     "lightgray", "black")

difInLonPlot <- ggplot(consolidated_df, aes(x = fund_sec_run)) +
  geom_segment(aes(x=reorder(fund_sec_run,sector),
                   xend=fund_sec_run,
                   y=LatitudeAnalysis.preDiffInLat2,
                   yend=LatitudeAnalysis.postDiffInLat2),
               linewidth = 1,
               color = line_color, 
               show.legend=FALSE) +
  geom_point(aes(y = LatitudeAnalysis.preDiffInLat2, shape = "Pre", color=funder), size = 2) +
  geom_point(aes(y = LatitudeAnalysis.postDiffInLat2, shape = "Post",color=funder), size = 2) +
  geom_hline(yintercept=0, color="gray") +
  scale_shape_manual(values = c("Pre" = 16, "Post" = 15),
                     breaks = c("Pre","Post")) +
  scale_color_manual(values = c("ch" = "red", "wb" = "blue", "both" = "purple"),
                     labels = c("China", "World Bank", "Both")) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Average Difference in Treated and Control Longitudes",
    shape = "IPW Adjustment",
    color = "Funder",
    title = "Pre- and Post- Inverse Probability Weighting",
    subtitle = "Average Difference in Treated and Control Longitudes") +
  theme_bw() +  
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/difInLon_xruns_",group_label_for_filename,".pdf"),
       difInLonPlot,
       width=8, height = 6, dpi=300,
       bg="white", units="in")


##############################################################################
# Out of sample error compared to baseline
##############################################################################  
line_color <- ifelse(abs(consolidated_df$ModelEvaluationMetrics.CELoss_out_baseline) > 
                       abs(consolidated_df$ModelEvaluationMetrics.CELoss_out),
                     "lightgray", "black")

difCELossPlot <- ggplot(consolidated_df, aes(x = fund_sec_run)) +
  geom_segment(aes(x=reorder(fund_sec_run,sector),
                   xend=fund_sec_run,
                   y=ModelEvaluationMetrics.CELoss_out_baseline,
                   yend=ModelEvaluationMetrics.CELoss_out),
               linewidth = 1,
               color = line_color, 
               show.legend=FALSE) +
  geom_point(aes(y = ModelEvaluationMetrics.CELoss_out_baseline, shape = "Baseline", color=funder), size = 2) +
  geom_point(aes(y = ModelEvaluationMetrics.CELoss_out, shape = "Model",color=funder), size = 2) +
  scale_shape_manual(values = c("Baseline" = 16, "Model" = 15),
                     breaks = c("Baseline","Model")) +
  scale_color_manual(values = c("ch" = "red", "wb" = "blue", "both" = "purple"),
                     labels = c("China", "World Bank", "Both")) +
  ylim(0,.7) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Out of Sample Error",
    shape = "CE Loss",
    color = "Funder",
    title = "Out of Sample Error") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/dif_CE_Loss_",group_label_for_filename,".pdf"),
       difCELossPlot,
       width=8, height = 6, dpi=300,
       bg="white", units="in")

##############################################################################
# Treatment Class Error, compared to baseline
##############################################################################  
line_color <- ifelse(abs(consolidated_df$ModelEvaluationMetrics.ClassError_out_baseline) > 
                       abs(consolidated_df$ModelEvaluationMetrics.ClassError_out),
                     "lightgray", "black")

dif_ClassError_plot <- ggplot(consolidated_df, aes(x = fund_sec_run)) +
  geom_segment(aes(x=reorder(fund_sec_run,sector),
                   xend=fund_sec_run,
                   y=ModelEvaluationMetrics.ClassError_out_baseline,
                   yend=ModelEvaluationMetrics.ClassError_out),
               linewidth = 1,
               color = line_color, 
               show.legend=FALSE) +
  geom_point(aes(y = ModelEvaluationMetrics.ClassError_out_baseline, shape = "Baseline", color=funder), size = 2) +
  geom_point(aes(y = ModelEvaluationMetrics.ClassError_out, shape = "Model",color=funder), size = 2) +
  scale_shape_manual(values = c("Baseline" = 16, "Model" = 15),
                     breaks = c("Baseline","Model")) +
  scale_color_manual(values = c("ch" = "red", "wb" = "blue", "both" = "purple"),
                     labels = c("China", "World Bank", "Both")) +
  ylim(0,.7) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Treatment Class Prediction Error (%)",
    shape = "CE Loss",
    color = "Funder",
    title = "Treatment Class Prediction Error") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/dif_ClassError_",group_label_for_filename,".pdf"),
       dif_ClassError_plot,
       width=8, height = 6, dpi=300,
       bg="white", units="in")
  
  
  
       





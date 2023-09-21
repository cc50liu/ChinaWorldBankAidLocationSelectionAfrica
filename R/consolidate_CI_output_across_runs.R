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
run_versions <- c("tfrec_cnn_agglom_v2","tfrec_emb_agglom_v2")
run_directories <-c("./results/tfrec_cnn_agglom_v2",
                    "./results/tfrec_emb_agglom_v2")
run_shortnames <- c("cnn","emb")
group_label_for_filename <- "cnn_emb_aggl"

#########################
#get matching files
#########################
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

#####################################################################
#consolidate run-level outputs, and add ridge regression coefficients
#####################################################################
read_and_process_file <- function(file) {
  #uncomment to test
  #file <-  "./results/tfrec_emb_agglom_v2/ICA_wb_160_tfrec_emb_agglom_v2_i1000.csv"
  
  #use a regular expression to construct ridge output file name corresponding
  #to the input file 
  ridge_file <- sub("(.*)(ICA_)(wb|ch|both)(_\\d+)(.*)_i\\d+.csv",
                    "\\1\\3\\4_tab_conf_compare\\5.csv", file)

  #read the primary data file
  data <- fread(file, header = TRUE, drop = grep("^(prW_est|SGD_loss)", colnames(fread(file))))
  
  #read the ridge data, convert to wide, and combine with primary data 
  ridge_long_dt <- fread(ridge_file, header = TRUE,select=c("term","ridge_est"))
  ridge_wide_dt <- dcast(ridge_long_dt, 1 ~ term, value.var="ridge_est")
  setnames(ridge_wide_dt, new = paste0("ridge_est.", names(ridge_wide_dt)))
  
  combined <- cbind(data,ridge_wide_dt)
  combined[, ridge_est.. := NULL]
  
  return(combined)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_dt <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)

#add/adjust variables
library(tidyr)
outcome_df <- consolidated_dt %>% 
  rename(fund_sec = fund_sect_param) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec),
         sig = ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.96, "***",
                      ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.645, "**", 
                             ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.282, "*", ""))),
         ate = paste0(round(tauHat_propensityHajek,2)," (",
                      round(tauHat_propensityHajek_se,2),")",sig),
         run_short = run_shortnames[match(run,run_versions)],
         fund_sec_run = paste0(fund_sec,"_",run_short),
         fund_run = paste0(funder,"_",run_short)
  ) %>%
  arrange(sector)  

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, ad_sector_names, sec_pre_name)

#determine sort order of Salience and Salience se columns to use in sort below
salience_cols <- grep("Salience",names(outcome_df),value=TRUE)
salience_vars <- sub("SalienceX.*\\.","",salience_cols)
sorted_sal_cols <- salience_cols[order(salience_vars)]

outcome_sector_df <- outcome_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(run,fund_sec,sec_pre_name,ad_sector_names,treat_count,control_count,nTrainableParameters,
         tauHat_propensityHajek,tauHat_propensityHajek_se,sig,tauHat_diffInMeans,
         all_of(sorted_sal_cols),starts_with("ridge_est"),
         starts_with("ModelEvaluation"), starts_with("Latitude"),
         everything())

names(outcome_sector_df)
write.csv(outcome_sector_df,paste0("./results/ICA_xruns_all_",group_label_for_filename,".csv"),
          row.names=FALSE)
#outcome_sector_df <- read.csv("./results/ICA_xruns_all_cnn_emb_aggl.csv")
#remove unneeded objects used to create this
rm(consolidated_dt,output)

###########################################
#consolidate location probabilities
###########################################
read_file_loc_probabilities <- function(file) {
  data <- fread(file, header = TRUE, select = grep("(prW_est|^run$|fund_sect_param)", colnames(fread(file))))
  return(data)
}

loc_probs_dt <- rbindlist(lapply(matching_files, read_file_loc_probabilities), 
                          use.names = TRUE, fill = TRUE)

#add/adjust variables
loc_probs_df <- loc_probs_dt %>% 
  rename(fund_sec = fund_sect_param) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec),
         run_short = run_shortnames[match(run,run_versions)],
         fund_sec_run = paste0(fund_sec,"_",run_short)
  ) %>%
  arrange(sector) %>% 
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(run,fund_sec,sector, funder,run_short, fund_sec_run, sec_pre_name, starts_with("prW")) 

write.csv(loc_probs_df,paste0("./results/ICA_xruns_all_probs_",group_label_for_filename,".csv"),
          row.names=FALSE)

loc_probs_df <- read.csv("./results/ICA_xruns_all_probs_cnn_emb_aggl.csv")

loc_probs_longer_df <- loc_probs_df %>% 
  select(fund_sec,run_short,starts_with("prW_est")) %>% 
  pivot_longer(cols=starts_with("prW_est")) %>% 
  pivot_wider(names_from=run_short) %>% 
  filter(complete.cases(.))

#write the correlations to a file
fund_sec_cor <- loc_probs_longer_df %>% 
  group_by(fund_sec) %>% 
  summarize(correlation=cor(cnn,emb)) %>% 
  arrange(correlation) 

  write.csv(fund_sec_cor,paste0("./results/corr_prob_xy_xruns_",group_label_for_filename,".csv"),
            row.names=FALSE)
  
  label_with_correlation <- function(fund_sec) {
    label <- fund_sec_cor %>%
      filter(fund_sec == fund_sec) %>%
      mutate(correlation = round(correlation,2)) %>% 
      pull(correlation)
    return(paste0(fund_sec," (corr ",label,")"))
  }

#plot the probabilities
prob_xy_plot <- loc_probs_longer_df %>% 
ggplot(aes(x = cnn, y = emb)) +
  geom_point(alpha=.1) +
  facet_wrap(~ fund_sec, labeller = as_labeller(label_with_correlation)) +
  labs(title = "Treatment Probabilities for DHS locations by funder_sector across models",
       x = "Convolutional Neural Network",
       y = "Random Embeddings") +   
  theme_bw()  +
  theme(panel.grid = element_blank())

#use png format for smaller filesize
ggsave(paste0("./results/prob_xy_xruns_",group_label_for_filename,".png"),
       prob_xy_plot,
       width=10, height = 8, dpi=300,
       bg="white", units="in")


###########################################
#prepare and write table display version
###########################################
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

###########################################
#generate figures
###########################################
#ate by sector funder and run 
ate_plot <- ggplot(outcome_sector_df,aes(x=tauHat_propensityHajek,
                                         y=fund_run,
                                         color=funder,
                                         shape=run_short)) +
  facet_grid(sec_pre_name ~ ., scales="free_y") +
  geom_pointrange(aes(xmin=tauHat_propensityHajek-(tauHat_propensityHajek_se*1.96),
                      xmax=tauHat_propensityHajek+(tauHat_propensityHajek_se*1.96))) +
  geom_point(aes(x=tauHat_diffInMeans,fill="baseline"), color="gray80") +
  # geom_segment(aes(x=min(tauHat_diffInMeans,tauHat_propensityHajek),
  #                  xend=max(tauHat_diffInMeans,tauHat_propensityHajek),
  #                  y=sec_pre_name,yend=sec_pre_name,color = "baseline"),
  #              linewidth = .5,
  #              position = position_jitterdodge(jitter.height=0.1)) +
  geom_vline(xintercept=0,color="gray80") +
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                     breaks = c("ch","wb","both"),
                     labels = c("ch" = "China","wb"="World Bank","both"="Both")) +
  scale_fill_manual(values=c("baseline"="gray80"),
                    labels=c("baseline"="No confounders")) +
  scale_shape_manual(values = c("cnn" = 16, "emb" = 17),
                     breaks = c("cnn","emb"),
                     labels = c("Convolutional Neural Net","Randomized Embeddings")) +
  labs(title = "Average Treatment Effect on Wealth, by Sector, Funder, and Model",
       x = "Estimated ATE with 95% confidence intervals",
       y = "",
       color="Funder",
       fill="Baseline estimate",
       shape="Model") +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0)) 


ggsave(paste0("./results/ate_funder_sector_xruns_",group_label_for_filename,".pdf"),
       ate_plot,
       width=10, height = 8, dpi=300,
       bg="white", units="in")


##############################################################################
# latitude differences, pre and post IPW, between treatment and control groups
##############################################################################
#create sector and funder columns, order by sector
# consolidated_df <- outcome_sector_df %>% 
#   mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sect_param)),
#          funder = sub("(wb|ch|both).*", "\\1", fund_sect_param),
#          run_short = run_shortnames[match(run,run_versions)],
#          fund_sec_run = paste0(fund_sect_param,"_",run_short)
#          ) %>% 
#   select(-starts_with("SGD_loss")) %>% 
#   arrange(sector) 

line_color <- ifelse(abs(outcome_sector_df$LatitudeAnalysis.preDiffInLat1) > 
                       abs(outcome_sector_df$LatitudeAnalysis.postDiffInLat1),
                     "gray92", "black")

difInLatPlot <- ggplot(outcome_sector_df, aes(x = fund_sec_run)) +
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
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                     breaks = c("ch","wb","both"),
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
line_color <- ifelse(abs(outcome_sector_df$LatitudeAnalysis.preDiffInLat2) > 
                       abs(outcome_sector_df$LatitudeAnalysis.postDiffInLat2),
                     "gray92", "black")

difInLonPlot <- ggplot(outcome_sector_df, aes(x = fund_sec_run)) +
  geom_segment(aes(x=reorder(fund_sec_run,sector),
                   xend=fund_sec_run,
                   y=LatitudeAnalysis.preDiffInLat2,
                   yend=LatitudeAnalysis.postDiffInLat2),
               linewidth = 1,
               color = line_color, 
               show.legend=FALSE) +
  geom_point(aes(y = LatitudeAnalysis.preDiffInLat2, shape = "Pre", color=funder), size = 2) +
  geom_point(aes(y = LatitudeAnalysis.postDiffInLat2, shape = "Post",color=funder), size = 2) +
  geom_hline(yintercept=0, color="gray80") +
  scale_shape_manual(values = c("Pre" = 16, "Post" = 15),
                     breaks = c("Pre","Post")) +
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                     breaks = c("ch","wb","both"),
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
line_color <- ifelse(abs(outcome_sector_df$ModelEvaluationMetrics.CELoss_out_baseline) > 
                       abs(outcome_sector_df$ModelEvaluationMetrics.CELoss_out),
                     "gray92", "black")

difCELossPlot <- ggplot(outcome_sector_df, aes(x = fund_sec_run)) +
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
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                     breaks = c("ch","wb","both"),
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
line_color <- ifelse(abs(outcome_sector_df$ModelEvaluationMetrics.ClassError_out_baseline) > 
                       abs(outcome_sector_df$ModelEvaluationMetrics.ClassError_out),
                     "gray92", "black")

dif_ClassError_plot <- ggplot(outcome_sector_df, aes(x = fund_sec_run)) +
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
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                     breaks = c("ch","wb","both"),
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
  
  
  
##############################################################################
# Compare tabular covariates salience across runs
##############################################################################  
var_order_all <- c("iwi_est_post_oda","log_pc_nl_pre_oda","log_avg_pop_dens",
                   "log_avg_min_to_city","agglomeration",
                   "log_dist_km_to_gold","log_dist_km_to_gems",        
                   "log_dist_km_to_dia","log_dist_km_to_petro", 
                   "leader_birthplace","log_trans_proj_cum_n",
                   "log_3yr_pre_conflict_deaths",
                   "polity2","log_gdp_per_cap_USD2015","country_gini","landsat57",
                   "landsat578")
var_labels_all <- c("Wealth (est, t+3)","Nightlights per capita (t-1,log)","Pop Density (t-1,log)",
                    "Minutes to City (2000,log)","Agglomeration (t-1)","Dist to Gold (km,log)",
                    "Dist to Gems (km,log)","Dist to Diam (km,log)",
                    "Dist to Oil (km,log)","Leader birthplace (t-1)","Prior Transport Projs",
                    "Conflict deaths (t-1,log)",
                    "Country Polity2 (t-1)","Cntry GDP/cap (t-1,log)","Country gini (t-1)",
                    "Landsat 5 & 7", "Landsat 5,7,& 8")

#to do:  handle the "ridge_est." columns here and put them on the figure as "tabular confounders only"
compare_salience_df <- outcome_sector_df %>%
  select(funder, sec_pre_name, ad_sector_names, run_short, starts_with("SalienceX"), starts_with("ridge_est")) %>% 
  mutate(sec_pre_name = case_match(sec_pre_name,
                                   "Developmental Food Aid/Food Security Assistance (520)" ~
                                     "Dev Food Aid/Food Security Assistance (520)",
                                   .default=sec_pre_name),
         ad_sector_names = case_match(ad_sector_names,
                                   "Developmental Food Aid/Food Security Assistance" ~
                                     "Dev Food Aid/Food Security Assistance",
                                   .default=ad_sector_names)) %>% 
  pivot_longer(cols=-c(funder,sec_pre_name, ad_sector_names, run_short)) %>% 
  separate_wider_delim(name,delim=".",names=c("measure","term")) %>% 
  pivot_wider(names_from = measure, values_from=value) %>% 
  filter(!grepl("cnty",term) & !grepl("landsat",term))

compare_salience_se_df <- compare_salience_df %>% 
  filter(!is.na(SalienceX_se)) 

compare_salience_no_se_df <- compare_salience_df %>% 
  filter(is.na(SalienceX_se)) 
         
#to do: come back to this to thinka bout the baseline point
all_dif_salience_plot <- ggplot(compare_salience_se_df, 
                            aes(x=SalienceX, y=ad_sector_names, shape=run_short, color=funder),
                            position=position_jitter(height = .25)) +
geom_pointrange(aes(xmin=SalienceX - (SalienceX_se*1.96),
                    xmax=SalienceX + (SalienceX_se*1.96)),
                position=position_jitter(height=0.25)) +
geom_point(data=compare_salience_no_se_df,
           aes(x=SalienceX, y=ad_sector_names, shape=run_short, color=funder)) +
geom_point(data=compare_salience_df,aes(x=ridge_est,y=ad_sector_names, 
                                        shape=run_short,fill="baseline"), color="gray80") +
scale_fill_manual(values=c("baseline"="gray80"),
                    labels=c("baseline"="No images")) +  
scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                   breaks = c("ch","wb","both"),
                   labels = c("China", "World Bank", "Both")) +
scale_shape_manual(values = c("cnn" = 16, "emb" = 21),
                   breaks = c("cnn","emb"),
                   labels = c("CNN","Emb")) +
geom_vline(xintercept=0, color="gray80") +
facet_wrap(~ factor(term, levels = var_order_all, labels = var_labels_all), scales = "fixed") +
labs(
  x = "Variable Salience",
  y = "",
  color = "Funder",
  shape = "Model",
  fill = "Baseline",
  title = "Salience of tabular variables across funders, models and sectors") +
theme_bw() +
theme(panel.grid = element_blank()) 


ggsave(paste0("./results/Salience_xruns_",group_label_for_filename,"_all.pdf"),
       all_dif_salience_plot,
       width=10.5, height = 9, dpi=600,
       bg="white", units="in")

  
##########################################################################
#define function to plot the salience of each variable in separate files
##########################################################################
plot_tab_confounder <- function(term_var) {
  #uncomment to test
  #term_var = "agglomeration"
  dif_salience_plot <- ggplot(compare_salience_se_df[compare_salience_se_df$term==term_var,], 
                                            aes(x=SalienceX, y=sec_pre_name, shape=run_short, color=funder),
                                            position=position_jitter(height = .25)) +
    geom_pointrange(aes(xmin=SalienceX - (SalienceX_se*1.96),
                        xmax=SalienceX + (SalienceX_se*1.96)),
                    position=position_jitter(height=0.25)) +
    geom_point(data=compare_salience_no_se_df[compare_salience_no_se_df$term==term_var,],
               aes(x=SalienceX, y=sec_pre_name, shape=run_short, color=funder)) +
#to do: fix this ridge estimate    
    geom_point(data=compare_salience_df,aes(x=ridge_est,y=sec_pre_name, 
                                            shape=run_short,fill="baseline"), color="gray80") +
    scale_fill_manual(values=c("baseline"="gray80"),
                      labels=c("baseline"="No images")) +
    scale_color_manual(values = c("ch" = "indianred1", "wb" = "lightblue1", "both" = "blueviolet"),
                       breaks = c("ch","wb","both"),
                       labels = c("China", "World Bank", "Both")) +
    scale_shape_manual(values = c("cnn" = 16, "emb" = 21),
                       breaks = c("cnn","emb"),
                       labels = c("CNN","Emb")) +
    geom_vline(xintercept=0, color="gray80") +
    labs(
      x = "Variable Salience",
      y = "",
      color = "Funder",
      shape = "Model",
      fill = "Baseline",
      title = paste0(var_labels_all[match(term_var,var_order_all)]," Salience across models and sectors")) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  
  ggsave(paste0("./results/Salience_xruns_",group_label_for_filename,"_",term_var,".pdf"),
         dif_salience_plot,
         width=10, height = 8, dpi=300,
         bg="white", units="in")
} 

#call function to write plots for each variable
lapply(unique(compare_salience_df$term), plot_tab_confounder)
  
  
#consolidate_CI_output_across_runs_cnn.R
# consolidate .csv files produced by AnalyzeCausalImages runs (after 9/9)
#   into a single csv
# v4 makes graphs for Latitude Analysis and ModelEvaluationMetrics
# parameter:  run version ("v9","v10",etc)
library(dplyr)
#use data.table since it handles missing columns
library(data.table)
library(ggplot2)
library(ggtext)

rm(list=ls())

run_versions <- c("cnn_3yr","emb_3yr")
run_directories <-c("./results/cnn_3yr",
                    "./results/emb_3yr")
run_shortnames <- c("cnn","emb")
group_label_for_filename <- "cnn_emb_3yr_2013"
group_label_for_titles <- "3-year images, 2002-2013"

#########################
#get matching files
#########################
get_matching_files <- function(run_version, run_directory) {
  #uncomment to test
  # run_version <- "cnn_5k_annual"
  # run_directory <- "./results/cnn_5k_annual"
  
  #filename_pattern <- paste0("ICA_",run_version,"_(wb|ch)_\\d{3}.csv")
  filename_pattern <- paste0("ICA_(wb|ch)_\\d{3}_",run_version,".*csv")
  
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
  #file <-  "./results/cnn_3yr/ICA_wb_140_cnn_3yr_i1000.csv"
  
  #read the primary data file
  data <- fread(file, header = TRUE, drop = grep("^(prW_est|SGD_loss|myGlmnet)", colnames(fread(file))))
  
  #use a regular expression to construct ridge coeff & ate file names based on 
  #input file.   
  ridge_coeffs <- sub("(.*)(ICA_)(wb|ch)(_\\d+)(.*)_i\\d+.csv",
                    "\\1\\3\\4_tab_conf_compare\\5.csv", file)  

  #read the ridge coeffs, convert to wide, and combine with primary data 
  ridge_long_dt <- fread(ridge_coeffs, header = TRUE,select=c("term","ridge_est"))
  ridge_wide_dt <- dcast(ridge_long_dt, 1 ~ term, value.var="ridge_est")
  setnames(ridge_wide_dt, new = paste0("ridge_est.", names(ridge_wide_dt)))
  
  combined <- cbind(data,ridge_wide_dt)
  combined[, ridge_est.. := NULL]

  #read the ridge ate and add a record on emb runs (didn't do bootstrap for cnn runs)
  if (grepl("emb",file)) {
    ridge_ate <- sub("(.*)(ICA_)(wb|ch)(_\\d+)(.*)_i\\d+.csv",
                   "\\1\\3\\4_ridge_tab_only.csv", file)  
  
    ridge_ate_df <- fread(ridge_ate, header=TRUE,select=c("fund_sect_param","ate_ridge","ate_se_ridge"))
    if (nrow(ridge_ate_df)==1) {
      combined_ridge <- bind_rows(combined,
                              tibble("run"="ridge",
                                     "run_short"="ridge",
                                     "fund_sect_param"=data[1]$fund_sect_param,
                                     "treat_count"=data[1]$treat_count,
                                     "control_count"=data[1]$control_count,
                                     "dropped_labels"=data[1]$dropped_labels,
                                     "tauHat_propensityHajek"=ridge_ate_df[1]$ate_ridge,
                                     "tauHat_propensityHajek_se"=ridge_ate_df[1]$ate_se_ridge,
                                     "tauHat_diffInMeans"=data[1]$tauHat_diffInMeans,
                                     "tauHat_diffInMeans_se"=data[1]$tauHat_diffInMeans_se))

      combined <- combined_ridge
    } #end of grepl("emb",file)
  } #end of !is.na(ridge_ate)
  
  return(combined)
}

# Read and merge the files, filling columns that don't exist in some with NAs
consolidated_dt <- rbindlist(lapply(matching_files, read_and_process_file), 
                             use.names = TRUE, fill = TRUE)

#add/adjust variables
library(tidyr)
outcome_df <- consolidated_dt %>% 
  select(-starts_with("SalienceX.adm2"),
         -starts_with("SalienceX_se.adm2"),
         -starts_with("trainIndices"),
         -starts_with("testIndices"),
         -starts_with("ridge_est.adm2"),
         -starts_with("tauHat_propensityHajek_vec"),
         -starts_with("myGlmnet_coefs")) %>% 
  rename(fund_sec = fund_sect_param) %>% 
  mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
         funder = sub("(wb|ch|both).*", "\\1", fund_sec),
         sig = ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.96, "***",
                      ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.645, "**", 
                             ifelse(abs(tauHat_propensityHajek) / tauHat_propensityHajek_se >= 1.282, "*", ""))),
         ate = paste0(round(tauHat_propensityHajek,2)," (",
                      round(tauHat_propensityHajek_se,2),")",sig),
         run_short = ifelse(run=="ridge","ridge",
                            run_shortnames[match(run,run_versions)]),
         fund_sec_run = paste0(fund_sec,"_",run_short),
         fund_run = paste0(funder,"_",run_short)
  ) %>%
  arrange(sector) 

sector_names_df <- read.csv("./data/interim/sector_group_names.csv") %>% 
  mutate(sec_pre_name = paste0(ad_sector_names," (",ad_sector_codes,")")) %>% 
  select(ad_sector_codes, ad_sector_names, sec_pre_name)

#determine sort order of Salience and Salience se columns to use in sort below
salience_cols <- grep("Salience|ridge_est.",names(outcome_df),value=TRUE)
salience_vars <- sub("SalienceX.*\\.|ridge_est\\.","",salience_cols)
sorted_sal_cols <- salience_cols[order(salience_vars)]

outcome_sector_df <- outcome_df %>%
  left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
  select(run,fund_sec,sector,sec_pre_name,ad_sector_names,treat_count,control_count,nTrainableParameters,
         tauHat_propensityHajek,tauHat_propensityHajek_se,sig,tauHat_diffInMeans,
         all_of(sorted_sal_cols),
         starts_with("ModelEvaluation"), starts_with("Latitude"),
         everything())

#create a version without ridge estimates for later plots
outcome_sector_noridge_df <- outcome_sector_df %>% filter(run!="ridge")

write.csv(outcome_sector_df,paste0("./results/ICA_xruns_all_",group_label_for_filename,".csv"),
          row.names=FALSE)
#outcome_sector_df <- read.csv("./results/ICA_xruns_all_cnn_emb_aggl.csv")
#remove unneeded objects used to create this
rm(consolidated_dt,output)

###########################################
#prepare and write table display version
###########################################
outcome_sector_display_df <- outcome_sector_df %>% 
  select(sec_pre_name, run_short, funder, ate) %>% 
  pivot_wider(names_from=funder, values_from=ate) %>% 
  rename(China=ch,
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
#order by ate
sector_order_ate <- outcome_sector_df %>% 
  group_by(sec_pre_name) %>% 
  summarize(max_ate=max(tauHat_propensityHajek)) %>% 
  arrange(desc(max_ate)) %>% 
  pull(sec_pre_name)

ate_plot <- ggplot(outcome_sector_df,
                   aes(x=tauHat_propensityHajek,
                       y=fund_run,
                       color=paste0(run_short,funder),
                       shape=paste0(run_short,funder))) +
  facet_grid(factor(sec_pre_name,levels=sector_order_ate) ~ ., scales="free_y") +
  geom_point(data=outcome_sector_df %>% filter(run_short!="ridge"),
             mapping=aes(x=tauHat_propensityHajek,
                         shape=paste0(run_short,funder)),size=.1) +
  geom_pointrange(data=outcome_sector_df %>% filter(run_short!="ridge"),
                  mapping=aes(xmin=tauHat_propensityHajek-(tauHat_propensityHajek_se*1.96),
                      xmax=tauHat_propensityHajek+(tauHat_propensityHajek_se*1.96))) +
  scale_color_manual(name="Funder & Approach",
                     values = c("cnnch"="indianred1","cnnwb"="mediumblue",
                                "embch"="indianred1","embwb"="mediumblue",
                                "ridgech"="indianred1","ridgewb"="mediumblue"),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb",
                                "China no images","WB no images")) +
  scale_shape_manual(name="Funder & Approach",
                     values = c("cnnch"=19,"cnnwb"=19,
                                "embch"=17,"embwb"=17,
                                "ridgech"=0,"ridgewb"=0),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb",
                                "China no images","WB no images")) +  
  labs(title = "Aid's Average Treatment Effect on Wealth, by Sector, Funder, and Vision Backbone",
       subtitle=group_label_for_titles,
       x = "Estimated ATE with 95% confidence intervals",
       y = "") +
  ggnewscale::new_scale_color() +
  geom_point(data=outcome_sector_df %>% filter(run_short=="ridge"),
             mapping=aes(x=tauHat_diffInMeans,color="baseline"), shape=15,size=1.3) +
  geom_point(data=outcome_sector_df %>% filter(run_short=="ridge"),
             mapping=aes(x=tauHat_propensityHajek,y=fund_run,
                 color=paste0(run_short,funder)), shape = 0, size=1.3) +
  geom_linerange(data=outcome_sector_df %>% filter(run_short=="ridge"),
                  mapping=aes(xmin=tauHat_propensityHajek-(tauHat_propensityHajek_se*1.96),
                      xmax=tauHat_propensityHajek+(tauHat_propensityHajek_se*1.96),
                      color=paste0(run_short,funder))) +
  geom_vline(xintercept=0,color="gray80") +  
  scale_color_manual(name="Baseline Estimates",
                     values=c("baseline"="gray60","ridgech"="indianred1","ridgewb"="mediumblue"),
                     breaks = c("baseline"),
                     labels=c("baseline"="No confounders")) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0))

ggsave(paste0("./results/ate_funder_sector_xruns_",group_label_for_filename,".pdf"),
       ate_plot,
       width=10, height = 8, dpi=600,
       bg="white", units="in")


##############################################################################
# Model AUC by sector, funder, run
##############################################################################
#order by auc
sector_order_auc <- outcome_sector_noridge_df %>% 
  group_by(sec_pre_name) %>% 
  summarize(max_auc=max(AUC)) %>% 
  arrange(desc(max_auc)) %>% 
  pull(sec_pre_name)

auc_plot <- ggplot(outcome_sector_df %>% filter(run!="ridge"),
                   aes(x=AUC,
                       y=fund_run,
                       color=paste0(run_short,funder),
                       shape=paste0(run_short,funder))) +
  facet_grid(factor(sec_pre_name,levels=sector_order_auc) ~ ., scales="free_y") +
  geom_point(size=3) +
  scale_x_continuous(breaks=c(.5,.6,.7,.8,.9,1),
                     limits=c(.45,1)) +
  geom_vline(xintercept=0.5,color="gray80") +  
  scale_color_manual(name="Funder & Backbone",
                     values = c("cnnch"="indianred1","cnnwb"="mediumblue",
                                "embch"="indianred1","embwb"="mediumblue",
                                "ridgech"="indianred1","ridgewb"="mediumblue"),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb",
                                "China no images","WB no images")) +
  scale_shape_manual(name="Funder & Backbone",
                     values = c("cnnch"=19,"cnnwb"=19,
                                "embch"=17,"embwb"=17,
                                "ridgech"=0,"ridgewb"=0),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb",
                                "China no images","WB no images")) +  
  labs(title = "Area Under the Curve (AUC) Model Prediction Quality",
       subtitle=group_label_for_titles,
       x = "AUC",
       y = "") +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),  
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y = element_text(angle = 0))

ggsave(paste0("./results/auc_funder_sector_xruns_",group_label_for_filename,".pdf"),
       auc_plot,
       width=10, height = 8, dpi=300,
       bg="white", units="in")


##############################################################################
# Cross entropy loss
##############################################################################  
#order by difference in baseline and model loss
run_order_celoss <- outcome_sector_noridge_df %>% 
  mutate(loss_difference=ModelEvaluationMetrics.CELoss_out - 
           ModelEvaluationMetrics.CELoss_out_baseline) %>% 
  arrange(desc(loss_difference)) %>% 
  pull(fund_sec_run)

outcome_sector_noridge_loss_order_df <- outcome_sector_noridge_df %>% 
  mutate(fund_sec_run = factor(fund_sec_run,levels=run_order_celoss))

difCELossPlot <- ggplot(outcome_sector_noridge_loss_order_df, 
                                 aes(x=fund_sec_run,
                                     color=paste0(run_short,funder),
                                     shape=paste0(run_short,funder))) +
  geom_segment(aes(y=ModelEvaluationMetrics.CELoss_out_baseline,
               yend=ModelEvaluationMetrics.CELoss_out,
               xend=fund_sec_run),
       linewidth = 1,
       color = "grey30",
       show.legend=FALSE,
       arrow=arrow(length=unit(.3,"cm"))) +
  geom_point(aes(y = ModelEvaluationMetrics.CELoss_out_baseline), size = 3) +
  #geom_point(aes(y = ModelEvaluationMetrics.CELoss_out, shape = "Model",color=funder), size = 2) +
  scale_color_manual(name="Baseline for\nFunder & Model",
                     values = c("cnnch"="indianred1","cnnwb"="mediumblue",
                                "embch"="indianred1","embwb"="mediumblue"),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb")) +
  scale_shape_manual(name="Baseline for\nFunder & Model",
                     values = c("cnnch"=19,"cnnwb"=19,
                                "embch"=17,"embwb"=17),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb")) +    
  labs(
      x = "Funder, Sector, and Vision Backbone, ordered by difference in CE Loss",
      y = "Cross Entropy Loss",
      title = "Cross Entropy Loss Comparison, Baseline and Image Confounding Models",
      subtitle=group_label_for_titles) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# Rotate x-axis labels by 45 degrees


ggsave(paste0("./results/dif_CE_Loss_",group_label_for_filename,".pdf"),
       difCELossPlot,
       width=10.5, height = 8, dpi=300,
       bg="white", units="in")

##############################################################################
# Treatment Class Error, compared to baseline
##############################################################################  
#order by difference in baseline and model loss
run_order_classerror <- outcome_sector_noridge_df %>% 
  mutate(classerror_difference=ModelEvaluationMetrics.ClassError_out - 
           ModelEvaluationMetrics.ClassError_out_baseline) %>% 
  arrange(desc(classerror_difference)) %>% 
  pull(fund_sec_run)

outcome_sector_noridge_classerror_order_df <- outcome_sector_noridge_df %>% 
  mutate(fund_sec_run = factor(fund_sec_run,levels=run_order_classerror))

dif_ClassError_plot <- ggplot(outcome_sector_noridge_classerror_order_df, 
                        aes(x=fund_sec_run,
                            color=paste0(run_short,funder),
                            shape=paste0(run_short,funder))) +
  geom_segment(aes(y=ModelEvaluationMetrics.ClassError_out_baseline,
                   yend=ModelEvaluationMetrics.ClassError_out,
                   xend=fund_sec_run),
               linewidth = 1,
               color = "grey30",
               show.legend=FALSE,
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_point(aes(y = ModelEvaluationMetrics.ClassError_out_baseline), size = 3) +
  scale_color_manual(name="Baseline for\nFunder & Model",
                     values = c("cnnch"="indianred1","cnnwb"="mediumblue",
                                "embch"="indianred1","embwb"="mediumblue"),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb")) +
  scale_shape_manual(name="Baseline for\nFunder & Model",
                     values = c("cnnch"=19,"cnnwb"=19,
                                "embch"=17,"embwb"=17),
                     labels = c("China CNN","World Bank CNN",
                                "China Emb","World Bank Emb")) +    
  labs(
    x = "Funder, Sector, and Vision Backbone, ordered by difference in Treatment Class Prediction Error",
    y = "Out of Sample Treatment Class Prediction Error (%)",
    title = "Out of Sample Treatment Class Prediction Error: Comparison of Baseline and Image Confounding Models",
    subtitle=group_label_for_titles) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/dif_ClassError_",group_label_for_filename,".pdf"),
       dif_ClassError_plot,
       width=10.5, height = 8, dpi=300,
       bg="white", units="in")



##############################################################################
# Compare tabular covariates salience across runs
##############################################################################  
var_order_all <- c("iwi_est_post_oda","log_pc_nl_pre_oda","log_avg_pop_dens",
                   "log_avg_min_to_city",
                   "log_dist_km_to_gold","log_dist_km_to_gems",        
                   "log_dist_km_to_dia","log_dist_km_to_petro", 
                   "leader_birthplace","log_ch_loan_proj_n",
                   "log_3yr_pre_conflict_deaths","log_disasters",
                   "election_year","unsc_aligned_us","unsc_non_aligned_us",
                   "country_gini",
                   "corruption_control", "gov_effectiveness", "political_stability",
                   "reg_quality", "rule_of_law","voice_accountability", 																				
                   "landsat578","log_treated_other_funder_n","log_other_sect_n",
                   "log_total_neighbor_projs")
var_labels_all <- c("Wealth","Nightlights per cap","Pop Density",
                    "Minutes to City", "Dist to Gold",
                    "Dist to Gems","Dist to Diam",
                    "Dist to Oil","Leader birthplace","China Loan Projs",
                    "Conflict deaths","Natural Disasters",
                    "Election year", "UNSC US Aligned","UNSC Non-US Align",
                    "Country gini",
                    "Cntry Cntrl Corruption", "Cntry Gov Effective",
                    "Cntry Political Stability","Cntry Reg Quality",
                    "Cntry Rule of Law","Cntry Voice/Account",
                    "Landsat 5,7,& 8","Other Funder Treat n","Other Sector Proj n",
                    "Adj ADM2 Proj n")

compare_salience_df <- outcome_sector_df %>%
  filter(run!="ridge") %>% 
  select(funder, sector, sec_pre_name, ad_sector_names, run_short, starts_with("SalienceX"), starts_with("ridge_est")) %>% 
  mutate(sec_pre_name = case_match(sec_pre_name,
                                   "Developmental Food Aid/Food Security Assistance (520)" ~
                                     "Dev Food Aid/Food Security Assistance (520)",
                                   .default=sec_pre_name),
         ad_sector_names = case_match(ad_sector_names,
                                      "Developmental Food Aid/Food Security Assistance" ~
                                        "Dev Food Aid/Food Security Assistance",
                                      .default=ad_sector_names)) %>% 
  pivot_longer(cols=-c(funder,sector,sec_pre_name, ad_sector_names, run_short)) %>% 
  separate_wider_delim(name,delim=".",names=c("measure","term")) %>% 
  pivot_wider(names_from = measure, values_from=value) %>% 
  filter(!grepl("adm2",term) & !grepl("landsat",term) & 
         !grepl("first_year",term) & !grepl("start_year",term))

# compare_salience_se_df <- compare_salience_df %>% 
#   filter(!is.na(SalienceX_se)) 
# 
# compare_salience_no_se_df <- compare_salience_df %>% 
#   filter(is.na(SalienceX_se)) 

all_dif_salience_plot <- ggplot(compare_salience_df) +
  geom_point(aes(x=SalienceX, y=as.factor(sector), shape=paste0(run_short,funder),
                 color=paste0(run_short,funder)),
             position=position_jitter(height = .25)) +
  scale_color_manual(name = "Funder & Backbone",
                     values = c("cnnch"="indianred1","cnnwb"="mediumblue","embch"="indianred1","embwb"="mediumblue"),
                     labels = c("China CNN","World Bank CNN","China Emb","World Bank Emb")) +
  scale_shape_manual(name = "Funder & Backbone",
                     values = c("cnnch"=19,"cnnwb"=19,"embch"=17,"embwb"=17),
                     labels = c("China CNN","World Bank CNN","China Emb","World Bank Emb")) +
  geom_vline(xintercept=0, color="gray80") +
  facet_wrap(~ factor(term, levels = var_order_all, labels = var_labels_all), scales = "fixed") +
  labs(
    x = "Variable Salience",
    y = "Sector",
    title = "Salience of tabular variables across funders, sectors, and backbones",
    subtitle = group_label_for_titles) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y=element_text(size=6)) 


ggsave(paste0("./results/Salience_xruns_",group_label_for_filename,"_all.pdf"),
       all_dif_salience_plot,
       width=10, height = 8, dpi=600,
       bg="white", units="in")

write.csv(compare_salience_df,paste0("./results/Salience_",group_label_for_filename,".csv"),
          row.names=FALSE)
##########################################################################
#define function to plot the salience of each variable in separate files
##########################################################################
plot_tab_confounder <- function(term_var) {
  #uncomment to test
  #term_var = "agglomeration"
  dif_salience_plot <- ggplot(compare_salience_df[compare_salience_df$term==term_var,], 
                              aes(x=SalienceX, y=sec_pre_name, shape=paste0(run_short,funder), color=paste0(run_short,funder)),
                              position=position_jitter(height = .25)) +
    geom_point(size=3) +
    scale_color_manual(name = "Funder & Backbone",
                       values = c("cnnch"="indianred1","cnnwb"="mediumblue","embch"="indianred1","embwb"="mediumblue"),
                       labels = c("China CNN","World Bank CNN","China Emb","World Bank Emb")) +
    scale_shape_manual(name = "Funder & Backbone",
                       values = c("cnnch"=19,"cnnwb"=19,"embch"=17,"embwb"=17),
                       labels = c("China CNN","World Bank CNN","China Emb","World Bank Emb")) +
    geom_vline(xintercept=0, color="gray80") +
    labs(
      x = "Variable Salience",
      y = "",
      fill = "Baseline",
      title = paste0(var_labels_all[match(term_var,var_order_all)],": Salience across sectors, funders, and backbones"),
      subtitle=group_label_for_titles) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  
  ggsave(paste0("./results/Salience_xruns_",group_label_for_filename,"_",term_var,".pdf"),
         dif_salience_plot,
         width=10, height = 8, dpi=300,
         bg="white", units="in")
} 

#call function to write plots for each variable
lapply(unique(compare_salience_df$term), plot_tab_confounder)

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
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "mediumblue"),
                     breaks = c("ch","wb"),
                     labels = c("China", "World Bank")) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Difference in Treated and Control Average Latitude",
    shape = "IPW Adjustment",
    color = "Funder",
    title = "Difference in Treated and Control Average Latitude, Pre- and Post- Inverse Probability Weighting",
    subtitle = group_label_for_titles) +
  theme_bw() +  
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/difInLat_xruns_",group_label_for_filename,".pdf"),
       difInLatPlot,
       width=10.5, height = 8, dpi=300,
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
  scale_color_manual(values = c("ch" = "indianred1", "wb" = "mediumblue"),
                     breaks = c("ch","wb","both"),
                     labels = c("China", "World Bank", "Both")) +
  labs(
    x = "Funder, Sector, and Run",
    y = "Difference in Treated and Control Average Longitude",
    shape = "IPW Adjustment",
    color = "Funder",
    title = "Difference in Treated and Control Average Longitude, Pre- and Post- Inverse Probability Weighting",
    subtitle=group_label_for_titles) +
  theme_bw() +  
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggsave(paste0("./results/difInLon_xruns_",group_label_for_filename,".pdf"),
       difInLonPlot,
       width=10.5, height = 8, dpi=300,
       bg="white", units="in")

###########################################
#consolidate location probabilities
###########################################
# read_file_loc_probabilities <- function(file) {
#   data <- fread(file, header = TRUE, select = grep("(prW_est|^run$|fund_sect_param)", colnames(fread(file))))
#   return(data)
# }
# 
# loc_probs_dt <- rbindlist(lapply(matching_files, read_file_loc_probabilities), 
#                           use.names = TRUE, fill = TRUE)
# 
# #add/adjust variables
# loc_probs_df <- loc_probs_dt %>% 
#   rename(fund_sec = fund_sect_param) %>% 
#   mutate(sector = as.integer(sub(".*_(\\d+).*", "\\1", fund_sec)),
#          funder = sub("(wb|ch|both).*", "\\1", fund_sec),
#          run_short = run_shortnames[match(run,run_versions)],
#          fund_sec_run = paste0(fund_sec,"_",run_short)
#   ) %>%
#   arrange(sector) %>% 
#   left_join(sector_names_df,join_by(sector==ad_sector_codes)) %>% 
#   select(run,fund_sec,sector, funder,run_short, fund_sec_run, sec_pre_name, starts_with("prW")) 
# 
# write.csv(loc_probs_df,paste0("./results/ICA_xruns_all_probs_",group_label_for_filename,".csv"),
#           row.names=FALSE)
# 
# loc_probs_longer_df <- loc_probs_df %>% 
#   select(fund_sec,run_short,starts_with("prW_est")) %>% 
#   pivot_longer(cols=starts_with("prW_est")) %>% 
#   pivot_wider(names_from=run_short) %>% 
#   filter(complete.cases(.))
# 
# #write the correlations to a file
# fund_sec_cor <- loc_probs_longer_df %>% 
#   group_by(fund_sec) %>% 
#   summarize(correlation=cor(.data[[run_shortnames[1]]],.data[[run_shortnames[2]]])) %>% 
#   arrange(correlation) 
# 
# write.csv(fund_sec_cor,paste0("./results/corr_prob_xy_xruns_",group_label_for_filename,".csv"),
#           row.names=FALSE)
# 
# label_with_correlation <- function(fund_sec) {
#   label <- fund_sec_cor %>%
#     filter(fund_sec == fund_sec) %>%
#     mutate(correlation = round(correlation,2)) %>% 
#     pull(correlation)
#   return(paste0(fund_sec," (corr ",label,")"))
# }
# 
# #plot the probabilities
# prob_xy_plot <- loc_probs_longer_df %>% 
#   ggplot(aes(x = .data[[run_shortnames[1]]], y = .data[[run_shortnames[2]]])) +
#   geom_point(alpha=.1) +
#   facet_wrap(~ fund_sec, labeller = as_labeller(label_with_correlation)) +
#   labs(title = "Treatment Probabilities for DHS locations by funder_sector across models",
#        subtitle=group_label_for_titles,
#        x = run_shortnames[1],
#        y = run_shortnames[2]) +   
#   theme_bw()  +
#   theme(panel.grid = element_blank())
# 
# #use png format for smaller filesize
# ggsave(paste0("./results/prob_xy_xruns_",group_label_for_filename,".png"),
#        prob_xy_plot,
#        width=10, height = 8, dpi=300,
#        bg="white", units="in")
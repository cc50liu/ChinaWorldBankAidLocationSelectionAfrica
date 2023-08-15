# main.R
# show sequence of scripts called for Image Confounding Analysis of WB and CH
# aid projects

#to do: think about whether I want to source this or just note for people to run manually
#what do I need to do to fully automate it?


##############################################################################
# Basic Data preparation
##############################################################################
source("./code/R/prep_projects.R", local=TRUE)
# read.csv("./data/all.csv") #ISO codes 
# read.csv("./data/AiddataWB1.4.2/projects.csv")
# read.csv("./data/AiddataWB1.4.2/locations.csv")
# read.csv("./data/AiddataWB1.4.2/projects_ancillary.csv") %>%

# write.csv(africa_isos_df,"./data/interim/africa_isos.csv",row.names = FALSE)
# write.csv("./data/interim/wb_iso3_mismatch.csv", row.names=FALSE)
# write.csv(wb_sect_group_df,"./data/interim/wb_africa_oda_sector_group.csv",row.names = FALSE)

# read.csv("./data/AiddataChinav1.1.1/GeoCoded_China_Data_Merged_Files/oda-like_flows.csv")
# write.csv("./data/interim/ch_iso3_adm0_mismatch.csv", row.names=FALSE)
# write.csv(ch_sect_group_df,"./data/interim/ch_africa_oda_sector_group.csv",row.names = FALSE)
# write.csv(oda_sect_group_df,"./data/interim/africa_oda_sector_group.csv",row.names = FALSE)
# write.csv(.,"./data/interim/sector_group_names.csv", row.names = FALSE)

source("./code/R/prep_dhs_points.R", local=TRUE)
#read.csv("./data/AIGlobalLab/dhs_clusters.csv") 
#write.csv(dhs_df,"./data/interim/dhs_clusters_id.csv",row.names=FALSE)
#write.csv(dhs_t_c_year,"./data/interim/dhs_treat_control_year.csv",row.names=FALSE)
#read.csv("./data/AIGlobalLab/incountry/bidirectional_resnet_lstm.csv") 
#write.csv(dhs_tc_est_df,"./data/interim/dhs_est_iwi.csv",row.names=FALSE)

##############################################################################
# Determine which points are treated and which are controls, by sector 
##############################################################################
source("./code/R/select_dhs_year_treat_control_sectorAfrica.R", local=TRUE)
# read.csv("./data/interim/dhs_est_iwi.csv")
# read.csv("./data/interim/wb_africa_oda_sector_group.csv") %>%
# read.csv("./data/interim/ch_africa_oda_sector_group.csv") %>%
# read.csv("./data/interim/africa_isos.csv")
# read.csv("./data/interim/dhs_est_iwi.csv") 
# read.csv("./data/interim/dhs_est_iwi.csv")

# write.csv(dhs_sect_prec_df,"./data/interim/dhs_treat_control_sect_prec_year.csv",row.names=FALSE) n=9608
# write.csv(dhs_sect_df,"./data/interim/dhs_treat_control_sector_year.csv",row.names=FALSE)  n=9608

##############################################################################
# Prepare confounder data for each dhs point / scene 
##############################################################################
source("./code/R/prep_confounders_dhs_raster.R", local=TRUE)
# read.csv("./data/interim/dhs_est_iwi.csv")
# write.csv(dhs_log_pd_df,"./data/interim/dhs_treat_control_raster.csv",row.names=FALSE)  n=9602

source("./code/R/prep_confounders_dhs_vector.R", local=TRUE)
# read.csv("./data/UCDP/GEDEvent_v23_1.csv") 
# read.csv("./data/interim/africa_isos.csv")
# read.csv("./data/interim/dhs_treat_control_raster.csv")
# write.csv(dhs_vector_df,"./data/interim/dhs_treat_control_vector.csv",row.names=FALSE)  n=9602

source("./code/R/prep_confounders_dhs_natl_res.R", local=TRUE)

source("./code/R/prep_confounders_dhs_country.R", local=TRUE)

source("./code/R/prep_confounders_dhs_loan_projects.R", local=TRUE)

source("./code/R/consolidate_confounders_dhs.R", local=TRUE)
# read.csv("./data/interim/dhs_treat_control_vector.csv") %>%
# read.csv("./data/interim/dhs_treat_control_raster.csv") %>%
# read.csv("./data/interim/dhs_treat_control_sector_year.csv")
#write.csv(dhs_confounders_df,"./data/interim/dhs_treat_control_confounders.csv",row.names=FALSE)

##############################################################################
# Call Causal Image Confounding Analysis, consolidate output files
##############################################################################
#balance dist of pre-project years in treatments and controls, and call analyzeimageconfounding
source("./code/R/call_CI_Conf_dhs_5bands_year_v4_cl_funder_sector.R", local=TRUE)
# read.csv("./data/interim/dhs_treat_control_sector_vector_year.csv")
# write.csv(output_df,paste0("./data/interim/ICA_",run,"_",orig_fund_sect_param,".csv")

#on server, reorder names of output files from CausalImages function
#sh rename_output.sh

#copy files to a results directory on laptop
#run script to convert png maps to pdfs and then consolidate into single pdf file
#combine_results_png_pdf.bat

#consolidate csv output for all sectors/funders into a single file
#create separate files to compare tabular and logistic regression for each funder/sector
source("./code/R/consolidate_CI_output.R", local=TRUE)

##############################################################################
# Maps, Charts and descriptive statistics 
##############################################################################
chart_dhs.R
#read.csv("./data/interim/dhs_treat_control_vector.csv")
#read.csv("./data/interim/sector_group_names.csv")
#read.csv("./data/interim/dhs_treat_control_raster.csv")

chart_projects.R:
# read.csv("./data/interim/africa_oda_sector_group.csv")
  
chart_treatment_assignment.R
#read.csv("./data/interim/dhs_treat_control_vector.csv")

chart_treatment_assignment_sector.R

source("./code/R/map_dhs_proj_countries.R", local=TRUE)

source("./code/R/prep_desc_stats.R", local=TRUE)
    
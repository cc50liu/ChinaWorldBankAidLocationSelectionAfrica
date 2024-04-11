# main.R
# This shows the environment setup and sequence of scripts I ran for the 
# Image Confounding Analysis of WB and CH aid projects
# I ran most of these manually and haven't tested running them automated through here

##############################################################################
# Configure NAISS high performance cluster environment apptainers
##############################################################################
#all steps below are from /mimer/NOBACKUP/groups/globalpoverty1/cindy/recipes/
#
# To rebuild the apptainer environment to include updates to the causalimages package:
# 1. Move current image to a backup name, so the new image can be created:
#    mv ../images/final_upg_causalimages.sif ../images/final_upg_causalimages.bck
# 2. Build the new image with Connor's updated software
#  apptainer build ../images/final_upg_causalimages.sif final_upg_causalimages.def &> final_upg_causalimages.log
# 3. Review logfile created by previous step to ensure there were no build errors
#
# To add additional R packages to the apptainer environment:
# 1. Edit the final.def file in the recipes directory to include the package in
#    the line starting with R --slave -e 'install.packages(c("areal"....
#    (keep them in alphabetical order to avoid duplicates)
# 2. Move current image to a backup name, so the new image can be created:
#    mv ../images/final.sif ../images/final.bck
# 3. Build the new image:
#     apptainer build ../images/final.sif final.def &> final.log
# 4. Review logfile created by previous step to ensure there were no build errors
# 5. Rebuild the final_upg_causalimages.sif using the steps in the first paragraph
#
# To create/update the python environment apptainer:
# 1. Move current image to a backup name, so the new image can be created:
#    mv ../images/cindy_python.sif ../images/cindy_python.bck
# 2. Build the new image with any python environment changes:
#    apptainer build ../images/cindy_python.sif cindy_python.def &> cindy_python.log
# 3. Review logfile created by previous step to ensure there were no build errors

##############################################################################
# Download images and per-capita nightlights from Google Earth Engine
##############################################################################
#download images over all DHS points, for a 30-year period using a python
# jupyter notebook:
# ./code/python/0_download_images.ipynb
#           uses ./code/python/gee_exporter.py
#                ./code/python/satelitte_sampling.py

#download csv file with per capita nightlight info over points used in this 
#study, from 2001-2013
#./code/python/0_download_percapita_nl_WorldPop.ipynb

##############################################################################
# Runs: 5K
##############################################################################
#Basic data prep
source("./code/R/prep_iso_codes.R", local=TRUE)
source("./code/R/prep_projects.R", local=TRUE)
source("./code/R/prep_dhs_points.R", local=TRUE) 

#Determine treatments/controls
#annual
source("./code/R/select_dhs_treat_control_5k.R", local=TRUE) 
#writes dhs_treated_sector_group_3yr.csv
#writes dhs_treated_sector_group_annual.csv
#writes dhs_treated_sector_annual.csv
#writes dhs_treated_sector_3yr.csv

#Prepare confounder Data for each dhs point/scene
#python  0_download_percapita_nl_harmonized_5k_WorldPop.ipynb
source("./code/R/prep_confounders_dhs_5k_raster.R", local=TRUE)
source("./code/R/prep_confounder_disasters.R", local=TRUE)
source("./code/R/prep_confounders_dhs_vector.R", local=TRUE)
source("./code/R/prep_confounders_dhs_natl_res.R", local=TRUE)
source("./code/R/prep_confounders_dhs_country.R", local=TRUE)
source("./code/R/prep_confounders_dhs_loan_projects.R", local=TRUE)
#consolidate all confounder data in a wide format, one row per dhs point
source("./code/R/consolidate_confounders_wide_5k_dhs.R", local=TRUE)

# Call Causal Image Confounding Analysis for each of the vision backbones
# and for 3year and annual images
#slurm scripts:
./code/scripts/run_emb_5k_3yr.sh
./code/scripts/run_cnn_5k_3yr.sh
./code/scripts/run_vt_5k_3yr.sh

./code/scripts/run_emb_5k_annual.sh
./code/scripts/run_cnn_5k_annual.sh
./code/scripts/run_vt_5k_annual.sh

#these scripts call either
# R/call_CI_Conf_5k_annual.R or
# R/call_CI_Conf_5k_3yr.R 
#with appropriate parameters for the specific run

#run will create a subdirectory named after the run, where all the output files will be
#for sector-based runs, run (from results directory):
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output.sh
#for sector-group based runs, run (from results directory):
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output_group.sh
#run sh scripts/rename_output.sh on server to rename output files for consolidation
#(another option:  scripts/rename_output_sector_groups.sh to group them into Infrastructure, 
#Interventions, BasicServices, and Other groups)

#copy files to a results directory on laptop
#run script to convert png files to pdfs and then consolidate into single pdf file for each funder
#change run name and funder below
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_emb_annual_s3_both_2002 wb . 
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_emb_annual_s3_both_2002 ch . 

#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 wb . 
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 ch . 

#can also combine only treatment propensity charts using combine_results_treatprop.bat

#consolidate results and prepare cross run figures
source("./code/R/consolidate_CI_output_across_runs_5k_annual.R") 


##############################################################################
# Runs:  tfrec_emb_group_annual, tfrec_cnn_group_annual
##############################################################################
#Basic data prep
source("./code/R/prep_projects_v2.R", local=TRUE)
#writes africa_oda_sector_group_v2.csv
source("./code/R/prep_dhs_points.R", local=TRUE)

#Determine treatments/controls
#annual
source("./code/R/select_dhs_treat_control_group_annual3.R", local=TRUE)
#writes dhs_treat_control_group_annual3.csv

#Prepare Confounder Data for each dhs point/scene
#python/0_download_percapita_nl_WorldPop.ipynb  
source("./code/R/prep_confounders_dhs_raster.R", local=TRUE)
source("./code/R/prep_confounders_dhs_vector.R", local=TRUE)
source("./code/R/prep_confounders_dhs_natl_res.R", local=TRUE)
source("./code/R/prep_confounders_dhs_country.R", local=TRUE)
source("./code/R/prep_confounders_dhs_loan_projects.R", local=TRUE)
#considate all confounder data in a wide format, one row per dhs point
source("./code/R/consolidate_confounders_wide_dhs.R", local=TRUE)

# Call Causal Image Confounding Analysis
source("./code/R/call_CI_Conf_tfrec_emb_group_annual.R", local=TRUE)
source("./code/R/call_CI_Conf_tfrec_cnn_group_annual.R", local=TRUE)

#shell scripts to submit slurm scripts every minute:  
#  scripts/run_tfrec_emb_group_annual.sh
#  scripts/run_tfrec_cnn_group_annual.sh
# which calls 
#  scripts/call_CI_Conf_tfrec_emb_group_annual.slurm
#  scripts/call_CI_Conf_tfrec_cnn_group_annual.slurm

#run will create a subdirectory named after the run, where all the output files will be
#for sector-based runs, run (from results directory):
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output.sh
#for sector-group based runs, run (from results directory):
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output_group.sh
#run sh scripts/rename_output.sh on server to rename output files for consolidation
#(another option:  scripts/rename_output_sector_groups.sh to group them into Infrastructure, 
#Interventions, BasicServices, and Other groups)

#copy files to a results directory on laptop
#run script to convert png files to pdfs and then consolidate into single pdf file for each funder
#change run name and funder below
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_emb_annual_s3_both_2002 wb . 
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_emb_annual_s3_both_2002 ch . 

#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 wb . 
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 ch . 

#can also combine only treatment propensity charts using combine_results_treatprop.bat

#consolidate results and prepare cross run figures
source("./code/R/consolidate_CI_output_across_runs_groups_annual.R")




##############################################################################
# Runs:  tfrec_emb_annual, tfrec_emb_agglom_v3, tfrec_cnn_agglom_v3
##############################################################################
#Basic data prep
source("./code/R/prep_projects_end_impute_median_or3.R", local=TRUE)
#writes africa_oda_end_impute_median_or3.csv
source("./code/R/prep_dhs_points.R", local=TRUE)

#Determine treatments/controls
#annual
source("./code/R/select_treat_control_annual.R", local=TRUE)
#writes dhs_treat_control_annual.csv

#collapsed
source("./code/R/select_treat_control_collapsed.R", local=TRUE)
#writes dhs_treat_control_collapsed.csv

#Prepare Confounder Data for each dhs point/scene
#same as runs above

# Call Causal Image Confounding Analysis
source("./code/R/call_CI_Conf_tfrec_emb_annual.R", local=TRUE)
source("./code/R/call_CI_Conf_tfrec_cnn_annual.R", local=TRUE) #create this
source("./code/R/call_CI_Conf_tfrec_emb_collapsed.R", local=TRUE)
source("./code/R/call_CI_Conf_tfrec_cnn_collapsed.R", local=TRUE) #create this

#Consolidate output files
#same steps as runs above

#consolidate results and prepare cross run figures
source("./code/R/consolidate_CI_output_across_runs_annual.R")
source("./code/R/consolidate_CI_output_across_runs_collapsed.R")
       

##############################################################################
# Run:  shallow_collapse
##############################################################################
#Basic data prep
source("./code/R/prep_projects_end_no_impute.R", local=TRUE)
source("./code/R/prep_dhs_points.R", local=TRUE)
#Determine treatments/controls
source("./code/R/select_dhs_year_treat_control_collapse_time_end.R", local=TRUE)
#writes a long format dhs_treat_control_collaps_end_dates.csv file 
# used in call_CI_Conf_dhs_shallow_collapse.R

#Prepare Confounder Data for each dhs point/scene
source("./code/R/prep_confounders_dhs_raster.R", local=TRUE)
source("./code/R/prep_confounders_dhs_vector.R", local=TRUE)
source("./code/R/prep_confounders_dhs_natl_res.R", local=TRUE)
source("./code/R/prep_confounders_dhs_country.R", local=TRUE)
source("./code/R/prep_confounders_dhs_loan_projects.R", local=TRUE)
#considate all confounder data in a wide format, one row per dhs point
source("./code/R/consolidate_confounders_wide_dhs.R", local=TRUE)

# Call Causal Image Confounding Analysis, consolidate output files
source("./code/R/call_CI_Conf_dhs_shallow_collapse.R", local=TRUE)
#shell script to submit slurm scripts every minute:  slurm/run_shallow_collapse.sh
# which calls call_shallow_collapse.slurm

#run will create a subdirectory named after the run, where all the output files will be
#run sh scripts/rename_output.sh on server to rename output files for consolidation
#(another option:  scripts/rename_output_sector_groups.sh to group them into Infrastructure, 
#Interventions, BasicServices, and Other groups)

#copy files to a results directory on laptop
#run script to convert png maps to pdfs and then consolidate into single pdf file
#combine_results_png_pdf.bat
#updated to create separate files by funder and subdirectory:

#can also combine only treatment propensity charts using combine_results_treatprop.bat

#create tables and handle tabular/numeric outputs
source("./code/R/consolidate_CI_output_v4.R", local=TRUE)

#to compare across runs, use consolidate_CI_output_across_runs.R

##############################################################################
# Run: others
##############################################################################
source("./code/R/prep_projects.R", local=TRUE)

source("./code/R/prep_dhs_points.R", local=TRUE)

##############################################################################
# Determine which points are treated and which are controls, by sector 
##############################################################################
source("./code/R/select_dhs_year_treat_control.R", local=TRUE)

##############################################################################
# Prepare confounder data for each dhs point / scene 
##############################################################################
source("./code/R/prep_confounders_dhs_raster.R", local=TRUE)
source("./code/R/prep_confounders_dhs_vector.R", local=TRUE)
source("./code/R/prep_confounders_dhs_natl_res.R", local=TRUE)
source("./code/R/prep_confounders_dhs_country.R", local=TRUE)
source("./code/R/prep_confounders_dhs_loan_projects.R", local=TRUE)

source("./code/R/consolidate_confounders_wide_dhs.R", local=TRUE)

##############################################################################
# Call Causal Image Confounding Analysis, consolidate output files
##############################################################################
#balance dist of pre-project years in treatments and controls, and call analyzeimageconfounding
#I normally don't call this script directly, but instead call it via a slurm script
#since it can take > 10 hours to run
source("./code/R/call_CI_Conf_dhs_long.R", local=TRUE)

#I have shell scripts that submit the slurm scripts for all funder/sector combinations
# every 10 minutes

#on server, reorder names of output files from CausalImages function
#sh rename_output.sh or 
#sh rename_output_sector_groups.sh

#copy files to a results directory on laptop
#run script to convert png maps to pdfs and then consolidate into single pdf file
#combine_results_png_pdf.bat

#consolidate csv output for all sectors/funders into a single file
#create separate files to compare tabular and logistic regression for each funder/sector
source("./code/R/consolidate_CI_output_v4.R", local=TRUE)

##############################################################################
# Maps, Charts and descriptive statistics 
##############################################################################
chart_dhs.R

chart_projects.R:
# read.csv("./data/interim/africa_oda_sector_group.csv")
  
chart_treatment_assignment.R
#read.csv("./data/interim/dhs_treat_control_vector.csv")

chart_treatment_assignment_sector.R

source("./code/R/map_dhs_proj_countries.R", local=TRUE)

source("./code/R/prep_desc_stats.R", local=TRUE)
    
# main.R
# show sequence of scripts called for Image Confounding Analysis of WB and CH
# aid projects
# I ran most of these manually and haven't tested running them automated through here

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

#after runs complete, create a directory below /results for the run and move output files there
#run sh slurm/rename_output.sh on server to rename output files for consolidation

#copy files to a results directory on laptop
#run script to convert png maps to pdfs and then consolidate into single pdf file
#combine_results_png_pdf.bat

#create separate files to compare tabular and logistic regression for each funder/sector
source("./code/R/consolidate_CI_output.R", local=TRUE)


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

#on server, create a directory below results for the run and move files there

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

chart_projects.R:
# read.csv("./data/interim/africa_oda_sector_group.csv")
  
chart_treatment_assignment.R
#read.csv("./data/interim/dhs_treat_control_vector.csv")

chart_treatment_assignment_sector.R

source("./code/R/map_dhs_proj_countries.R", local=TRUE)

source("./code/R/prep_desc_stats.R", local=TRUE)
    
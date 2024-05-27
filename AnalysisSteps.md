## Analysis Steps
The environment setup and sequence of scripts I ran for the Image Confounding Analysis of World Bank and China aid projects in Africa are described below.

### 1. Configure NAISS high performance cluster environment apptainers
*I ran the following from the /mimer/NOBACKUP/groups/globalpoverty1/cindy/recipes/ directory.  The scripts below are stored in the ./env_conf directory.  If rerunning, adjust the commands for your directory structure.*

#### To rebuild the apptainer environment to include updates to the causalimages package:
1. Move current image to a backup name, so the new image can be created:
   mv ../images/final_upg_causalimages.sif ../images/final_upg_causalimages.bck
2. Build the new image with Connor's updated software
  apptainer build ../images/final_upg_causalimages.sif final_upg_causalimages.def &> final_upg_causalimages.log
3. Review logfile created by previous step to ensure there were no build errors

#### To add additional R packages to the apptainer environment:
1. Edit the final.def file in the recipes directory to include the package in
   the line starting with R --slave -e 'install.packages(c("areal"....
   (keep them in alphabetical order to avoid duplicates)
2. Move current image to a backup name, so the new image can be created:
   mv ../images/final.sif ../images/final.bck
3. Build the new image:
    apptainer build ../images/final.sif final.def &> final.log
4. Review logfile created by previous step to ensure there were no build errors
5. Rebuild the final_upg_causalimages.sif using the steps in the first paragraph

#### To create/update the python environment apptainer:
1. Move current image to a backup name, so the new image can be created:
   mv ../images/cindy_python.sif ../images/cindy_python.bck
2. Build the new image with any python environment changes:
   apptainer build ../images/cindy_python.sif cindy_python.def &> cindy_python.log
3. Review logfile created by previous step to ensure there were no build errors


### 2. Download images and per-capita nightlights from Google Earth Engine
1. To download daytime satellite images over all neighborhood units of analysis for the study period via a jupyter notebook:

   ./python/0_download_images_c1_5k_3yr.ipynb
   
   This relies upon
   -  ./python/satellite_sampling_5k_3y.py
   -  ./python/gee_exporter_c1_5k.py

   *Note that I also downloaded images from collection 2, of different resolutions, and of different timeframes, so those scripts are also available in the same directory.* 

2. To download a csv file with per capita nightlight info over points used in this study
   ./python/0_download_percapita_nl_harmonized_5k_WorldPop.ipynb

### 3. Data Preparation
1. Prepare basic data used by the rest of the analysis:
   - ./R/prep_iso_codes.R
   - ./R/prep_projects.R
   - ./R/prep_dhs_points.R 

3. Determine which neighborhoods are treatments or controls for each funder and sector
   - ./R/select_dhs_treat_control_5k.R

4. Prepare confounder data for each neighborhood 
   - ./R/prep_confounders_dhs_5k_raster.R
   - ./R/prep_confounder_disasters.R
   - ./R/prep_confounders_dhs_vector.R
   - ./R/prep_confounders_dhs_natl_res.R
   - ./R/prep_confounders_dhs_country.R
   - ./R/prep_confounders_dhs_loan_projects.R
     
   To consolidate all confounder data in a wide format, one row per dhs point
   - ./R/consolidate_confounders_wide_5k_dhs.R

### 4. Prepare Descriptive Statistics, Maps, Charts, and Figures
*Note that descriptive statistics specific to a funder-sector are produced by each funder-sector analysis in step 5.  The steps below are for all other figures and tables in the study.*

- ./R/prep_desc_stats.R
- ./R/chart_projects.R
- ./R/chart_dhs_projs.R
- ./R/map_dhs_proj_countries.R

### 5. Call Causal Image Confounding Analysis for each of the vision backbones
1. For the Convolutional Neural Network analysis on 3-year images
   - ./scripts/run_cnn_3yr.sh is a shell script that submits a funder/sector analysis each minute 
   - by calling the SLURM script ./scripts/call_CI_Conf_5k_3yr.slurm
   - which calls the R script ./R/call_CI_Conf_5k_3yr.R
2. For the Randomized Embedding analysis on 3-year images
   - ./scripts/run_emb_3yr.sh is a shell script that submits a funder/sector analysis each minute 
   - by calling the SLURM script ./scripts/call_CI_Conf_5k_3yr.slurm
   - which calls the R script ./R/call_CI_Conf_5k_3yr.R

In addition to running the Image Confounding Analysis, this R script also
1. Constructs the final panel data for each sector and funder
2. Writes the tfrecord files to speed up the analysis
3. Produces the maps, boxplots, scatterplots and other funder/sector specific figures
4. Runs a Ridge Regression on the tabular-only covariates, produces a treatment propensity overlap chart, and estimates an Average Treatment Effect with 100 bootstrap samples and takes the standard deviation to estimate the standard error.

*Note that the directory also has parallel scripts to run the analysis on annual images and to run with the vision transformer backbone, which did not make it into the thesis due to time and space constraints.

These runs create a subdirectory named after the run, where all the output files are placed.  The output files for this thesis are in the following locations:
- ./results/cnn_3yr
- ./results/emb_3yr

### 6. Process results 
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output.sh
#for sector-group based runs, run (from results directory):
#   sh ../../ChinaWorldBankAidLocationSelectionAfrica/scripts/rename_output_group.sh
#run sh scripts/rename_output.sh on server to rename output files for consolidation
#(another option:  scripts/rename_output_sector_groups.sh to group them into Infrastructure, 
#Interventions, BasicServices, and Other groups)

#copy files to a results directory on laptop
#run script to convert png files to pdfs and then consolidate into single pdf file for each funder
#change run name and funder below
#..\..\code\scripts\combine_results_png_pdf.bat cnn_3yr wb . 
#..\..\code\scripts\combine_results_png_pdf.bat cnn_3yr ch . 

#..\..\code\scripts\combine_results_png_pdf.bat emb_3yr wb . 
#..\..\code\scripts\combine_results_png_pdf.bat emb_3yr ch . 

#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 wb . 
#..\..\code\scripts\combine_results_png_pdf.bat tfrec_cnn_annual_s3_both_2002 ch . 

#can also combine 
# - only treatment propensity charts using combine_results_treatprop.bat
# - both loss and treatment propensities using combine_loss_hist.bat

#consolidate results and prepare cross run figures
source("./code/R/consolidate_CI_output_across_runs_cnn.R") 
source("./code/R/consolidate_CI_output_across_runs_5k_annual.R") 




    

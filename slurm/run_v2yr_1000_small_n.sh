#!/bin/bash

# Submit slurm scripts every 10 minutes
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

# Education
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_110 v2yr 1000

# Health
submit_and_wait small_n_cl_sector_dhs_long.slurm both_120 v2yr 1000

# Water Supply and Sanitation
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_140 v2yr 1000

# Government and Civil Society
submit_and_wait small_n_cl_sector_dhs_long.slurm both_150 v2yr 1000


# Other Social Infrastructure and Services
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_160 v2yr 1000

# Sector Group: Economic Infrastructure & Services
# Transport and Storage
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_210 v2yr 1000
submit_and_wait small_n_cl_sector_dhs_long.slurm both_210 v2yr 1000

# Communications
submit_and_wait small_n_cl_sector_dhs_long.slurm wb_220 v2yr 1000
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_220 v2yr 1000

# Energy Generation and Supply
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_230 v2yr 1000

# Banking and Financial Services
submit_and_wait small_n_cl_sector_dhs_long.slurm wb_240 v2yr 1000

# Sector Group: Production
# Agriculture, Forestry and Fishing
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_310 v2yr 1000

# Trade and Tourism
submit_and_wait small_n_cl_sector_dhs_long.slurm wb_330 v2yr 1000

# Industry, Mining, Construction
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_320 v2yr 1000

# General Environmental Protection
submit_and_wait small_n_cl_sector_dhs_long.slurm wb_410 v2yr 1000

# Other Multisector
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_430 v2yr 1000

#Developmental Food Aid/Food Security Assistance
submit_and_wait small_n_cl_sector_dhs_long.slurm ch_520 v2yr 1000

submit_and_wait small_n_cl_sector_dhs_long.slurm ch_700 v2yr 1000

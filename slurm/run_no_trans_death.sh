#!/bin/bash

# Submit slurm scripts every 10 minutes
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 5m     # Sleep for 5 minutes
}

# Education
submit_and_wait cl_sector_dhs.slurm wb_110 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_110 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_110 no_trans_death 1000

# Health
submit_and_wait cl_sector_dhs.slurm wb_120 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_120 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_120 no_trans_death 1000

# Water Supply and Sanitation
submit_and_wait cl_sector_dhs.slurm ch_140 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm wb_140 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_140 no_trans_death 1000

# Government and Civil Society
submit_and_wait cl_sector_dhs.slurm wb_150 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_150 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_150 no_trans_death 1000

# Other Social Infrastructure and Services
submit_and_wait cl_sector_dhs.slurm wb_160 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_160 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_160 no_trans_death 1000

# Sector Group: Economic Infrastructure & Services
# Transport and Storage
submit_and_wait cl_sector_dhs.slurm wb_210 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_210 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_210 no_trans_death 1000

# Communications
submit_and_wait cl_sector_dhs.slurm wb_220 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_220 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_220 no_trans_death 1000

# Energy Generation and Supply
submit_and_wait cl_sector_dhs.slurm wb_230 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm ch_230 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_230 no_trans_death 1000

# Banking and Financial Services
submit_and_wait cl_sector_dhs.slurm wb_240 no_trans_death 1000

# Sector Group: Production
# Agriculture, Forestry and Fishing
submit_and_wait cl_sector_dhs.slurm wb_310 no_trans_death 1000
submit_and_wait cl_sector_dhs.slurm both_310 no_trans_death 1000

# Trade and Tourism
submit_and_wait cl_sector_dhs.slurm wb_330 no_trans_death 1000

# Industry, Mining, Construction
submit_and_wait cl_sector_dhs.slurm wb_320 no_trans_death 1000

# General Environmental Protection
submit_and_wait cl_sector_dhs.slurm wb_410 no_trans_death 1000

# Other Multisector
submit_and_wait cl_sector_dhs.slurm ch_430 no_trans_death 1000

#Developmental Food Aid/Food Security Assistance
submit_and_wait cl_sector_dhs.slurm ch_520 no_trans_death 1000

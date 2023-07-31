#!/bin/bash

# Submit slurm scripts every 20 minutes
submit_and_wait() {
  sbatch "$1" "$2"   # Submit the Slurm script with parameters
  sleep 20m     # Sleep for 20 minutes
}

# Education
#submit_and_wait cl_sector_v12_100_dhs.slurm wb_110
submit_and_wait cl_sector_v12_100_dhs.slurm ch_110
submit_and_wait cl_sector_v12_100_dhs.slurm both_110

# Health
submit_and_wait cl_sector_v12_100_dhs.slurm wb_120
submit_and_wait cl_sector_v12_100_dhs.slurm ch_120
submit_and_wait cl_sector_v12_100_dhs.slurm both_120

# Water Supply and Sanitation
submit_and_wait cl_sector_v12_100_dhs.slurm ch_140
submit_and_wait cl_sector_v12_100_dhs.slurm wb_140
submit_and_wait cl_sector_v12_100_dhs.slurm both_140

# Government and Civil Society
submit_and_wait cl_sector_v12_100_dhs.slurm wb_150
submit_and_wait cl_sector_v12_100_dhs.slurm ch_150
submit_and_wait cl_sector_v12_100_dhs.slurm both_150

# Other Social Infrastructure and Services
submit_and_wait cl_sector_v12_100_dhs.slurm wb_160
submit_and_wait cl_sector_v12_100_dhs.slurm ch_160
submit_and_wait cl_sector_v12_100_dhs.slurm both_160

# Sector Group: Economic Infrastructure & Services
# Transport and Storage
submit_and_wait cl_sector_v12_100_dhs.slurm wb_210
submit_and_wait cl_sector_v12_100_dhs.slurm ch_210
submit_and_wait cl_sector_v12_100_dhs.slurm both_210

# Communications
submit_and_wait cl_sector_v12_100_dhs.slurm wb_220
submit_and_wait cl_sector_v12_100_dhs.slurm ch_220
submit_and_wait cl_sector_v12_100_dhs.slurm both_220

# Energy Generation and Supply
submit_and_wait cl_sector_v12_100_dhs.slurm wb_230
submit_and_wait cl_sector_v12_100_dhs.slurm ch_230
submit_and_wait cl_sector_v12_100_dhs.slurm both_230

# Banking and Financial Services
submit_and_wait cl_sector_v12_100_dhs.slurm wb_240

# Sector Group: Production
# Agriculture, Forestry and Fishing
submit_and_wait cl_sector_v12_100_dhs.slurm wb_310
submit_and_wait cl_sector_v12_100_dhs.slurm both_310

# Trade and Tourism
submit_and_wait cl_sector_v12_100_dhs.slurm wb_330

# Industry, Mining, Construction
submit_and_wait cl_sector_v12_100_dhs.slurm wb_320

# General Environmental Protection
submit_and_wait cl_sector_v12_100_dhs.slurm wb_410

# Other Multisector
submit_and_wait cl_sector_v12_100_dhs.slurm ch_430

#Developmental Food Aid/Food Security Assistance
submit_and_wait cl_sector_v12_100_dhs.slurm ch_520

#!/bin/bash

# Submit slurm scripts every 10 minutes
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 1s     # Sleep 1 second
}

# Education
submit_and_wait cl_sector_png_test.slurm wb_110 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_110 png_test 1
submit_and_wait cl_sector_png_test.slurm both_110 png_test 1

# Health
submit_and_wait cl_sector_png_test.slurm wb_120 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_120 png_test 1
submit_and_wait cl_sector_png_test.slurm both_120 png_test 1

# Water Supply and Sanitation
submit_and_wait cl_sector_png_test.slurm ch_140 png_test 1
submit_and_wait cl_sector_png_test.slurm wb_140 png_test 1
submit_and_wait cl_sector_png_test.slurm both_140 png_test 1

# Government and Civil Society
submit_and_wait cl_sector_png_test.slurm wb_150 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_150 png_test 1
submit_and_wait cl_sector_png_test.slurm both_150 png_test 1

# Other Social Infrastructure and Services
submit_and_wait cl_sector_png_test.slurm wb_160 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_160 png_test 1
submit_and_wait cl_sector_png_test.slurm both_160 png_test 1

# Sector Group: Economic Infrastructure & Services
# Transport and Storage
submit_and_wait cl_sector_png_test.slurm wb_210 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_210 png_test 1
submit_and_wait cl_sector_png_test.slurm both_210 png_test 1

# Communications
submit_and_wait cl_sector_png_test.slurm wb_220 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_220 png_test 1
submit_and_wait cl_sector_png_test.slurm both_220 png_test 1

# Energy Generation and Supply
submit_and_wait cl_sector_png_test.slurm wb_230 png_test 1
submit_and_wait cl_sector_png_test.slurm ch_230 png_test 1
submit_and_wait cl_sector_png_test.slurm both_230 png_test 1

# Banking and Financial Services
submit_and_wait cl_sector_png_test.slurm wb_240 png_test 1

# Sector Group: Production
# Agriculture, Forestry and Fishing
submit_and_wait cl_sector_png_test.slurm wb_310 png_test 1
submit_and_wait cl_sector_png_test.slurm both_310 png_test 1

# Trade and Tourism
submit_and_wait cl_sector_png_test.slurm wb_330 png_test 1

# Industry, Mining, Construction
submit_and_wait cl_sector_png_test.slurm wb_320 png_test 1

# General Environmental Protection
submit_and_wait cl_sector_png_test.slurm wb_410 png_test 1

# Other Multisector
submit_and_wait cl_sector_png_test.slurm ch_430 png_test 1

#Developmental Food Aid/Food Security Assistance
submit_and_wait cl_sector_png_test.slurm ch_520 png_test 1

#!/bin/bash

# Submit slurm scripts every 10 minutes
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

# Education
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_110 v2yr 1000

# Health
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_120 v2yr 1000
submit_and_wait big_n_cl_sector_dhs_long.slurm ch_120 v2yr 1000

# Water Supply and Sanitation
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_140 v2yr 1000

# Government and Civil Society
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_150 v2yr 1000

# Other Social Infrastructure and Services
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_160 v2yr 1000

# Sector Group: Economic Infrastructure & Services
# Transport and Storage
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_210 v2yr 1000

# Energy Generation and Supply
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_230 v2yr 1000

# Banking and Financial Services
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_240 v2yr 1000

# Sector Group: Production
# Agriculture, Forestry and Fishing
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_310 v2yr 1000

# Industry, Mining, Construction
submit_and_wait big_n_cl_sector_dhs_long.slurm wb_320 v2yr 1000

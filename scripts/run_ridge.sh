#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_ridge_funder_sector.slurm ch_700 
submit_and_wait call_ridge_funder_sector.slurm ch_140 
submit_and_wait call_ridge_funder_sector.slurm ch_230 
submit_and_wait call_ridge_funder_sector.slurm ch_220 
submit_and_wait call_ridge_funder_sector.slurm ch_310 
submit_and_wait call_ridge_funder_sector.slurm ch_160 
submit_and_wait call_ridge_funder_sector.slurm wb_330 
submit_and_wait call_ridge_funder_sector.slurm wb_410 
submit_and_wait call_ridge_funder_sector.slurm ch_110 
submit_and_wait call_ridge_funder_sector.slurm ch_210 
submit_and_wait call_ridge_funder_sector.slurm wb_240 
submit_and_wait call_ridge_funder_sector.slurm wb_220 
submit_and_wait call_ridge_funder_sector.slurm ch_150 
submit_and_wait call_ridge_funder_sector.slurm ch_120 
submit_and_wait call_ridge_funder_sector.slurm wb_320 
submit_and_wait call_ridge_funder_sector.slurm wb_230 
submit_and_wait call_ridge_funder_sector.slurm wb_110 
submit_and_wait call_ridge_funder_sector.slurm wb_120 
submit_and_wait call_ridge_funder_sector.slurm wb_310 
submit_and_wait call_ridge_funder_sector.slurm wb_160 
submit_and_wait call_ridge_funder_sector.slurm wb_140 
submit_and_wait call_ridge_funder_sector.slurm wb_210 
submit_and_wait call_ridge_funder_sector.slurm wb_150 

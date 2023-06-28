#!/usr/bin/env bash
#SBATCH -A SNIC2022-3-38 -p alvis      	#project
#SBATCH -N 1 --gpus-per-node=A100fat:1  # 1 node on Tesla A100fat GPUs
#SBATCH -t 2-00:00:00                  	#max walltime job should run - 2 days

timestamp=$(date +'%Y%m%d%H%M%S')

# Define output and error filenames with timestamp
output_file="/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/logs/slurm5b2000idhs_output_${timestamp}.log"
error_file="/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/logs/slurm5b2000idhs_error_${timestamp}.log"

#SBATCH --output=$output_file
#SBATCH --error=$error_file

apptainer exec --nv /mimer/NOBACKUP/groups/globalpoverty1/cindy/images/final_upg_causalimages.sif Rscript /mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/code/R/call_CausalImage_Confounding_dhs_5bands.R
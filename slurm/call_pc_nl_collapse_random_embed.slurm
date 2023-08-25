#!/usr/bin/env bash
#SBATCH -A SNIC2022-3-38 -p alvis      	#project
#SBATCH -N 1 --gpus-per-node=A40:1      # 1 node on A40
#SBATCH -t 0-10:00:00                  	#max walltime job should run - 10 hours
#SBATCH -o "/mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/logs/%j.out"

start_time=$(date +%s)
FUNDER_SECTOR=$1
RUN_NAME=$2
ITERATIONS=$3

apptainer exec --nv /mimer/NOBACKUP/groups/globalpoverty1/cindy/images/final_upg_causalimages.sif Rscript /mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/ChinaWorldBankAidLocationSelectionAfrica/R/call_CI_Conf_per_capita_nl_collapse_rand_emb.R "$FUNDER_SECTOR" "$RUN_NAME" "$ITERATIONS" 

end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo "Total Elapsed Time: $(date -u -d @"$elapsed_time" +"%H:%M:%S") for $FUNDER_SECTOR, run $RUN_NAME, iter: $ITERATIONS"

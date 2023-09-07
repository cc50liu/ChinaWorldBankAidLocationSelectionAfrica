#!/usr/bin/env bash
#SBATCH -A SNIC2022-3-38 -p alvis      	#project
#SBATCH -N 1 --gpus-per-node=A40:1      # 1 node on A40
#SBATCH -t 0-8:00:00                  	#max walltime job should run - 8 hours

echo "Command-line arguments: $@"
start_time=$(date +%s)

FUNDER_SECTOR=$@

apptainer exec --nv /mimer/NOBACKUP/groups/globalpoverty1/cindy/images/final_upg_causalimages.sif Rscript /mimer/NOBACKUP/groups/globalpoverty1/cindy/eoml_ch_wb/ChinaWorldBankAidLocationSelectionAfrica/R/call_CI_Conf_dhs_v12_syn.R "$FUNDER_SECTOR"

end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo "Total Elapsed Time: $(date -u -d @"$elapsed_time" +"%H:%M:%S") for $FUNDER_SECTOR"

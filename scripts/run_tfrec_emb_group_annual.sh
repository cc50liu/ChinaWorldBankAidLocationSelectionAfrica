#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5"  # Submit the Slurm script with parameters
  sleep 1m # Sleep for 1 minute
}

submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm both_PRO tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm ch_OTH tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm ch_DIR tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm both_EIS tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm ch_PRO tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm wb_OTH tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm ch_EIS tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm both_SIS tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm ch_SIS tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm wb_PRO tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm wb_EIS tfrec_emb_group_annual 1000 annual
submit_and_wait call_CI_Conf_tfrec_emb_group_annual.slurm wb_SIS tfrec_emb_group_annual 1000 annual
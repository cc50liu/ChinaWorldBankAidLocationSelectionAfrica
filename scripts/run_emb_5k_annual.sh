#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5"  # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

#submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_430 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_520 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_700 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_140 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_230 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_220 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_310 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_160 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_330 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_410 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_110 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_240 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_210 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_220 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_150 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_320 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_230 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_110 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm ch_120 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_120 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_310 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_160 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_140 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_210 emb_5k_annual 1000 annual
submit_and_wait call_CI_Conf_emb_5k_annual.slurm wb_150 emb_5k_annual 1000 annual


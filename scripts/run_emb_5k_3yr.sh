#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5" "$6" # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_CI_Conf_5k_3yr.slurm ch_430 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_520 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_700 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_140 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_230 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_220 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_310 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_160 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_330 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_410 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_110 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_210 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_240 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_220 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_150 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_120 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_320 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_230 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_110 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_120 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_310 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_160 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_140 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_210 emb_5k_3yr 1000 3yr emb
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_150 emb_5k_3yr 1000 3yr emb
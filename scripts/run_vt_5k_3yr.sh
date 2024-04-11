#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5" "$6"  # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_CI_Conf_5k_3yr.slurm ch_430 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_520 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_700 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_140 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_230 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_220 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_310 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_160 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_330 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_410 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_110 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_210 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_240 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_220 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_150 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_120 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_320 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_230 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_110 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_120 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_310 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_160 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_140 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_210 vt_5k_3yr 1000 3yr vt
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_150 vt_5k_3yr 1000 3yr vt
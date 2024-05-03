#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5" "$6" # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_CI_Conf_5k_annual.slurm ch_430 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_520 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_700 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_140 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_230 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_220 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_310 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_160 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_330 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_410 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_110 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_240 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_210 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_220 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_150 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_320 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_230 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_110 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm ch_120 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_120 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_310 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_160 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_140 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_210 cnn_5k_annual 1000 annual cnn
submit_and_wait call_CI_Conf_5k_annual.slurm wb_150 cnn_5k_annual 1000 annual cnn


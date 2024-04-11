#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5" "$6"  # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_CI_Conf_5k_3yr.slurm ch_430 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_520 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_700 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_140 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_230 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_220 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_310 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_160 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_330 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_410 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_110 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_210 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_240 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_220 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_150 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm ch_120 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_320 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_230 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_110 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_120 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_310 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_160 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_140 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_210 cnn_5k_3yr 1000 3yr cnn
submit_and_wait call_CI_Conf_5k_3yr.slurm wb_150 cnn_5k_3yr 1000 3yr cnn
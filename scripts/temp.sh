#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5" "$6"  # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

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

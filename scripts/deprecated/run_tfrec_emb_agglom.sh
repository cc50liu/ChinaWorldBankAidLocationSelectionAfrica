#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4" "$5"  # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

#submit_and_wait call_CI_Conf_emb_agglom.slurm both_110 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_110 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_110 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_120 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_120 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_120 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_140 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_140 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_140 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_150 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_150 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_150 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_160 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_160 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_160 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_210 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_210 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_210 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_220 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_220 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_220 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_230 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_230 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_230 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_240 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm both_310 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_310 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_320 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_330 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm wb_410 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_430 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_520 tfrec_emb_agglom 1000 collapsed
submit_and_wait call_CI_Conf_emb_agglom.slurm ch_700 tfrec_emb_agglom 1000 collapsed

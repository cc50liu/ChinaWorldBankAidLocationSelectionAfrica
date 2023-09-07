#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_randomizedEmbeds.slurm both_110 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_110 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_110 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_120 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_120 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_120 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_140 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_140 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_140 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_150 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_150 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_150 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_160 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_160 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_160 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_210 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_210 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_210 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_220 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_220 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_220 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_230 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_230 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_230 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_240 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm both_310 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_310 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_320 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_330 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm wb_410 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_430 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_520 rand_emb 1000
submit_and_wait call_randomizedEmbeds.slurm ch_700 rand_emb 1000
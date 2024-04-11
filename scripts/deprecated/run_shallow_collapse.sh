#!/bin/bash

# Submit slurm scripts every minute
submit_and_wait() {
  sbatch "$1" "$2" "$3" "$4"   # Submit the Slurm script with parameters
  sleep 1m     # Sleep for 1 minute
}

submit_and_wait call_shallow_collapse.slurm both_110 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_110 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_110 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_120 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_120 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_120 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_140 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_140 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_140 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_150 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_150 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_150 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_160 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_160 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_160 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_210 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_210 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_210 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_220 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_220 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_220 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_230 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_230 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_230 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_240 sh_co 1000
submit_and_wait call_shallow_collapse.slurm both_310 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_310 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_310 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_320 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_320 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_330 sh_co 1000
submit_and_wait call_shallow_collapse.slurm wb_410 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_430 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_520 sh_co 1000
submit_and_wait call_shallow_collapse.slurm ch_700 sh_co 1000
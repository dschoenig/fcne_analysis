#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=32G
#SBATCH --time=9:00:00
#SBATCH --array=1-53
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

Rscript 4_evaluate_lp.R AMZ $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

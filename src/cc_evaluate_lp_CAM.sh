#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=42G
#SBATCH --time=15:00:00
#SBATCH --array=1-6
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

Rscript 4_evaluate_lp.R CAM $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

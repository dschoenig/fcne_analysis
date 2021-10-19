#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=20G
#SBATCH --time=05:30:00
#SBATCH --array=1-100
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4d_evaluate_posterior_mar_cam.R $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

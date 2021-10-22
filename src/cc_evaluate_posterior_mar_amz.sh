#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=32G
#SBATCH --time=08:00:00
#SBATCH --array=1-100
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4c_evaluate_posterior_mar_amz.R $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

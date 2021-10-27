#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=38G
#SBATCH --time=07:00:00
#SBATCH --array=1-200
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4_evaluate_lp.R CAM marginal $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

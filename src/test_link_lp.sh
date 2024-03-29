#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=36G
#SBATCH --time=02:30:00
#SBATCH --array=1-3
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript test_link_lp.R CAM $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

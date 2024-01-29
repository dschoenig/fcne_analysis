#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=28G
#SBATCH --time=5:00:00
#SBATCH --array=1-100%50
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

Rscript 4_evaluate_predictions.R AMZ $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_COUNT

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=16
#SBATCH --mem=12G
#SBATCH --time=15:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 1a_fit_som.R AMZ $SLURM_CPUS_PER_TASK

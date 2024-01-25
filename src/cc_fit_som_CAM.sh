#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=16
#SBATCH --mem=8G
#SBATCH --time=06:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 gcc/9.3.0 gdal/3.5.1 geos/3.10.2 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 1a_fit_som.R CAM $SLURM_CPUS_PER_TASK

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=148G
#SBATCH --time=15:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=amz1def

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 2_fit_gam.R AMZ 1 def $SLURM_CPUS_PER_TASK


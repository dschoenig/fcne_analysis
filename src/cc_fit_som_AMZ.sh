#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=16
#SBATCH --mem=8G
#SBATCH --time=06:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 1a_fit_som.R AMZ $SLURM_CPUS_PER_TASK

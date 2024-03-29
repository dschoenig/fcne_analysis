#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=124G
#SBATCH --time=23:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=AMZ_6

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 2_fit_gam.R AMZ 6 $SLURM_CPUS_PER_TASK


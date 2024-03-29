#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 2_fit_gam.R CAM 0 $SLURM_CPUS_PER_TASK
Rscript 2_fit_gam.R CAM 1 $SLURM_CPUS_PER_TASK
Rscript 2_fit_gam.R CAM 2 $SLURM_CPUS_PER_TASK

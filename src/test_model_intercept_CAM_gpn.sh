#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=76G
#SBATCH --time=02:55:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript test_model_intercept.R CAM gpn $SLURM_CPUS_PER_TASK


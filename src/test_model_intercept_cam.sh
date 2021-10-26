#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=96G
#SBATCH --time=05:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript test_model_intercept_cam.R CAM 3 $SLURM_CPUS_PER_TASK

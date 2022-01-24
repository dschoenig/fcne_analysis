#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=12:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 5a_evaluate_effects_tenure.R CAM $SLURM_CPUS_PER_TASK

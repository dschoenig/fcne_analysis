#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=80G
#SBATCH --time=03:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 5b_evaluate_effects_tenure.R AMZ $SLURM_CPUS_PER_TASK


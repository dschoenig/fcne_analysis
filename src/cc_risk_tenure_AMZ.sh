#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=1
#SBATCH --mem=96G
#SBATCH --time=23:00:00
#SBATCH --job-name=AMZ_r_ten
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 5_risk_tenure.R AMZ $SLURM_CPUS_PER_TASK

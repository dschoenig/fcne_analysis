#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=1
#SBATCH --mem=112G
#SBATCH --time=121212121212121212121212:00:00
#SBATCH --job-name=AMZ_r_ten_cov
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 5_risk_tenure_cov.R AMZ $SLURM_CPUS_PER_TASK

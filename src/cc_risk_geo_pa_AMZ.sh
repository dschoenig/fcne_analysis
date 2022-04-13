#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=1
#SBATCH --mem=80G
#SBATCH --time=08:00:00
#SBATCH --job-name=AMZ_r_geo_pa
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 5_risk_geo.R AMZ 4 $SLURM_CPUS_PER_TASK
Rscript 5_risk_geo.R AMZ 5 $SLURM_CPUS_PER_TASK

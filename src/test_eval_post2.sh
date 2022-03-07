#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=48G
#SBATCH --time=15:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2

Rscript 4_evaluate_lp.R AMZ 51 200

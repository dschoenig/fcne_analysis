#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=40G
#SBATCH --time=06:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4_evaluate_lp.R AMZ marginal 1 200

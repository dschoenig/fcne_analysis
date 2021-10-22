#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=36G
#SBATCH --time=04:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4c_evaluate_posterior_mar_amz.R 1 200

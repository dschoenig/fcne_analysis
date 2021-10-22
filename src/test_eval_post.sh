#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --mem-per-cpu=32G
#SBATCH --time=01:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

Rscript 4a_evaluate_posterior_mar_amz.R 1999 5000
Rscript 4b_evaluate_posterior_mar_amz.R 1999 5000
Rscript 4c_evaluate_posterior_mar_amz.R 1999 5000
Rscript 4d_evaluate_posterior_mar_amz.R 1999 5000

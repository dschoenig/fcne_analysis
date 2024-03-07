#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=72G
#SBATCH --time=15:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=amz_mar_tena_full_ov

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal.R 4 ten.areas amz af full no_ov
Rscript 6_marginal.R 4 ten.areas amz pf full no_ov
Rscript 6_marginal.R 4 ten.areas amz af full ov
Rscript 6_marginal.R 4 ten.areas amz pf full ov

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=112G
#SBATCH --time=96:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=amz_mar_tena_pa

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal_effects.R 4 ten.areas amz af pa_mar all
Rscript 6_marginal_effects.R 4 ten.areas amz pf pa_mar all
Rscript 6_marginal_effects.R 4 ten.areas amz af pa_mar ov
Rscript 6_marginal_effects.R 4 ten.areas amz pf pa_mar ov

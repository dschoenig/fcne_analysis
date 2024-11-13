#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=18:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=amz_mar_ten_comp

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz def rec all
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz deg rec all
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz def rec ov
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz deg rec ov
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz def ind all
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz deg ind all
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz def ind ov
Rscript 6_marginal_effects_no_drought.R 4 ten_comp amz deg ind ov

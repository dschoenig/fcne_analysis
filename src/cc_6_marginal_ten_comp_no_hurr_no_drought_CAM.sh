#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=18:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cam_mar_ten_comp_nh

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam def rec all no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam deg rec all no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam def rec ov no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam deg rec ov no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam def ind all no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam deg ind all no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam def ind ov no_hurr
Rscript 6_marginal_effects_no_drought.R 4 ten_comp cam deg ind ov no_hurr

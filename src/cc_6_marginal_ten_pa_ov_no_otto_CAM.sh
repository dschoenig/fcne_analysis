#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=72G
#SBATCH --time=15:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cam_mar_ten_pa_ov_no

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal_effects.R 4 ten cam af pa no_ov no_otto
Rscript 6_marginal_effects.R 4 ten cam pf pa no_ov no_otto
Rscript 6_marginal_effects.R 4 ten cam af pa ov no_otto
Rscript 6_marginal_effects.R 4 ten cam pf pa ov no_otto

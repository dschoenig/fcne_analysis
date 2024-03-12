#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=5:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_geo_amz

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 5_counterfactual_geo.R 4 amz af it
Rscript 5_counterfactual_geo.R 4 amz af pa
Rscript 5_counterfactual_geo.R 4 amz af itpa

Rscript 5_counterfactual_geo.R 4 amz pf it
Rscript 5_counterfactual_geo.R 4 amz pf pa
Rscript 5_counterfactual_geo.R 4 amz pf itpa

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=72G
#SBATCH --time=10:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_ten_amz

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 5_counterfactual_ten.R 4 amz af it all
Rscript 5_counterfactual_ten.R 4 amz af it no_ov
Rscript 5_counterfactual_ten.R 4 amz af it ov

Rscript 5_counterfactual_ten.R 4 amz af pa all
Rscript 5_counterfactual_ten.R 4 amz af pa no_ov
Rscript 5_counterfactual_ten.R 4 amz af pa ov

Rscript 5_counterfactual_ten.R 4 amz af itpa all


Rscript 5_counterfactual_ten.R 4 amz pf it all
Rscript 5_counterfactual_ten.R 4 amz pf it no_ov
Rscript 5_counterfactual_ten.R 4 amz pf it ov

Rscript 5_counterfactual_ten.R 4 amz pf pa all
Rscript 5_counterfactual_ten.R 4 amz pf pa no_ov
Rscript 5_counterfactual_ten.R 4 amz pf pa ov

Rscript 5_counterfactual_ten.R 4 amz pf itpa all

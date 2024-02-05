#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=10:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_ten_cam

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 5_counterfactual_ten.R 4 af cam it all
Rscript 5_counterfactual_ten.R 4 af cam it no_ov
Rscript 5_counterfactual_ten.R 4 af cam it ov

Rscript 5_counterfactual_ten.R 4 af cam pa all
Rscript 5_counterfactual_ten.R 4 af cam pa no_ov
Rscript 5_counterfactual_ten.R 4 af cam pa ov

Rscript 5_counterfactual_ten.R 4 af cam itpa all


Rscript 5_counterfactual_ten.R 4 pf cam it all
Rscript 5_counterfactual_ten.R 4 pf cam it no_ov
Rscript 5_counterfactual_ten.R 4 pf cam it ov

Rscript 5_counterfactual_ten.R 4 pf cam pa all
Rscript 5_counterfactual_ten.R 4 pf cam pa no_ov
Rscript 5_counterfactual_ten.R 4 pf cam pa ov

Rscript 5_counterfactual_ten.R 4 pf cam itpa all

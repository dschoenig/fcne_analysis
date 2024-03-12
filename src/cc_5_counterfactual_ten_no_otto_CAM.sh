#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=10:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_ten_no_otto_cam

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 5_counterfactual_ten.R 4 cam af it all no_otto
Rscript 5_counterfactual_ten.R 4 cam af it no_ov no_otto
Rscript 5_counterfactual_ten.R 4 cam af it ov no_otto

Rscript 5_counterfactual_ten.R 4 cam af pa all no_otto
Rscript 5_counterfactual_ten.R 4 cam af pa no_ov no_otto
Rscript 5_counterfactual_ten.R 4 cam af pa ov no_otto

Rscript 5_counterfactual_ten.R 4 cam af itpa all no_otto


Rscript 5_counterfactual_ten.R 4 cam pf it all no_otto
Rscript 5_counterfactual_ten.R 4 cam pf it no_ov no_otto
Rscript 5_counterfactual_ten.R 4 cam pf it ov no_otto

Rscript 5_counterfactual_ten.R 4 cam pf pa all no_otto
Rscript 5_counterfactual_ten.R 4 cam pf pa no_ov no_otto
Rscript 5_counterfactual_ten.R 4 cam pf pa ov no_otto

Rscript 5_counterfactual_ten.R 4 cam pf itpa all no_otto

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=72G
#SBATCH --time=12:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cf_ten_areas_no_cam

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 5_counterfactual_ten_areas.R 4 cam af full all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam af full no_ov no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam af full ov no_otto

Rscript 5_counterfactual_ten_areas.R 4 cam af it_mar all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam af it_mar ov no_otto

Rscript 5_counterfactual_ten_areas.R 4 cam af pa_mar all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam af pa_mar ov no_otto

Rscript 5_counterfactual_ten_areas.R 4 cam pf full all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam pf full no_ov no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam pf full ov no_otto

Rscript 5_counterfactual_ten_areas.R 4 cam pf it_mar all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam pf it_mar ov no_otto

Rscript 5_counterfactual_ten_areas.R 4 cam pf pa_mar all no_otto
Rscript 5_counterfactual_ten_areas.R 4 cam pf pa_mar ov no_otto

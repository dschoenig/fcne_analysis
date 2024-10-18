#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=1
#SBATCH --mem=36G
#SBATCH --time=01:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 arrow/15.0.1 thrift/0.19.0 r/4.3.1

Rscript 7_model_summary.R AMZ def
Rscript 7_model_summary.R AMZ deg
Rscript 7_model_summary.R CAM def
Rscript 7_model_summary.R CAM deg

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=36:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cam_fl

Rscript 6_forest_loss.R 4 cam af
Rscript 6_forest_loss.R 4 cam pf
Rscript 6_forest_loss.R 4 cam af no_otto
Rscript 6_forest_loss.R 4 cam pf no_otto

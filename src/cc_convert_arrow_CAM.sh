#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=1
#SBATCH --mem=12G
#SBATCH --time=03:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
Rscript 4b_convert_arrow.R CAM $SLURM_CPUS_PER_TASK

DIR_LP="/home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp/"
rm -rf ${DIR_LP}
mv ${DIR_LP}.arrow ${DIR_LP}
# Rename for CentOS
rename --verbose cam.lp- cam.lp-00 ${DIR_LP}*/cam.lp-?.arrow
rename --verbose cam.lp- cam.lp-0 ${DIR_LP}*/cam.lp-??.arrow

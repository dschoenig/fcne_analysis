#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=1:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
ulimit -n 2048

mv /home/schoed/scratch/fcne_analysis/models/gam/lp/amz.lp \
        /home/schoed/scratch/fcne_analysis/models/gam/lp/amz.lp_old
mv /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp \
        /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp_old

Rscript 4b_reorganize_lp_data.R $SLURM_CPUS_PER_TASK

rm -rf /home/schoed/scratch/fcne_analysis/models/gam/lp/*.lp_old

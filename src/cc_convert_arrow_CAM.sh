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

rm -rf /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp
mv /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp.arrow /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp
rename 's/\d+/sprintf("%03d",$&)/e' /home/schoed/scratch/fcne_analysis/models/gam/lp/cam.lp/*/*.arrow

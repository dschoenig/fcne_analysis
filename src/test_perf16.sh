#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=34G
#SBATCH --time=05:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.2
export OMP_NUM_THREADS=$SLRUM_CPUS_PER_TASK
Rscript test_perf.R CAM t16 $SLURM_CPUS_PER_TASK

#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=5:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

# module load StdEnv/2020 gcc/9.3.0 gdal/3.2.3 geos/3.9.1 udunits/2.2.26 r/4.1.2

# export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
# Rscript 7_visualization_cov.R AMZ $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_maps.R AMZ $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_tenure_adm.R AMZ $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_tenure_dist.R AMZ $SLURM_CPUS_PER_TASK

# Rscript 7_visualization_cov.R CAM $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_maps.R CAM $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_tenure_adm.R CAM $SLURM_CPUS_PER_TASK
# Rscript 7_visualization_tenure_dist.R CAM $SLURM_CPUS_PER_TASK

# Rscript 7_visualization_cov.R AMZ 4
# Rscript 7_visualization_maps.R AMZ 4
# Rscript 7_visualization_tenure_adm.R AMZ 4
# Rscript 7_visualization_tenure_dist.R AMZ 4

# Rscript 7_visualization_cov.R CAM 4
Rscript 7_visualization_maps.R CAM 4
# Rscript 7_visualization_tenure_adm.R CAM 4
# Rscript 7_visualization_tenure_dist.R CAM 4

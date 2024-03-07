#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=72G
#SBATCH --time=5:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=mar_amz

module load StdEnv/2023 gcc/12.3 gdal/3.7.2 geos/3.12.0 python/3.11.5 udunits/2.2.28 r/4.3.1

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

Rscript 6_marginal.R 4 geo amz af it NA
Rscript 6_marginal.R 4 geo amz pf it NA

Rscript 6_marginal.R 4 geo amz af pa NA
Rscript 6_marginal.R 4 geo amz pf pa NA

Rscript 6_marginal.R 4 geo amz af itpa NA
Rscript 6_marginal.R 4 geo amz pf itpa NA


Rscript 6_marginal.R 4 ten amz af it all
Rscript 6_marginal.R 4 ten amz pf it all

Rscript 6_marginal.R 4 ten amz af it no_ov
Rscript 6_marginal.R 4 ten amz pf it no_ov
Rscript 6_marginal.R 4 ten amz af it ov
Rscript 6_marginal.R 4 ten amz pf it ov

Rscript 6_marginal.R 4 ten amz af pa all
Rscript 6_marginal.R 4 ten amz pf pa all

Rscript 6_marginal.R 4 ten amz af pa no_ov
Rscript 6_marginal.R 4 ten amz pf pa no_ov
Rscript 6_marginal.R 4 ten amz af pa ov
Rscript 6_marginal.R 4 ten amz pf pa ov

Rscript 6_marginal.R 4 ten amz af itpa all
Rscript 6_marginal.R 4 ten amz pf itpa all


Rscript 6_marginal 4 ten.areas amz af full all
Rscript 6_marginal 4 ten.areas amz pf full all

Rscript 6_marginal 4 ten.areas amz af full no_ov
Rscript 6_marginal 4 ten.areas amz pf full no_ov
Rscript 6_marginal 4 ten.areas amz af full ov
Rscript 6_marginal 4 ten.areas amz pf full ov

Rscript 6_marginal 4 ten.areas amz af it_mar all
Rscript 6_marginal 4 ten.areas amz pf it_mar all
Rscript 6_marginal 4 ten.areas amz af it_mar ov
Rscript 6_marginal 4 ten.areas amz pf it_mar ov

Rscript 6_marginal 4 ten.areas amz af pa_mar all
Rscript 6_marginal 4 ten.areas amz pf pa_mar all
Rscript 6_marginal 4 ten.areas amz af pa_mar ov
Rscript 6_marginal 4 ten.areas amz pf pa_mar ov

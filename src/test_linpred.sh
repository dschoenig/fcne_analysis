#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=32G
#SBATCH --time=01:00:00
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020 r/4.1.0

# Export the nodes names. 
# If all processes are allocated on the same node, NODESLIST contains : node1 node1 node1 node1
# Cut the domain name and keep only the node name
export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
Rscript test_linpred.R

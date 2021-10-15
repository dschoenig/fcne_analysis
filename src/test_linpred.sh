#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --ntasks-per-node=1
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=16G
#SBATCH --time=01:00:00

module load StdEnv/2020 r/4.1.0

# Export the nodes names. 
# If all processes are allocated on the same node, NODESLIST contains : node1 node1 node1 node1
# Cut the domain name and keep only the node name
export NODESLIST=$(echo $(srun hostname | cut -f 1 -d '.'))
R -f test_linpred.R

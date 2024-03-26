#!/bin/bash

#SBATCH --job-name=trace-f
#SBATCH --output=%j.trace-f.out
#SBATCH --error=%j.trace-f.err
#SBATCH --time=01:00:00
#SBATCH --partition=fm-bigmem-1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G

###Load necessary module
module load R/4.1.2

###Run script
Rscript trace-f.R

###Unload loaded modules
module unload R/4.1.2

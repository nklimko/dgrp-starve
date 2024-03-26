#!/bin/bash

#SBATCH --job-name=stepwise-f
#SBATCH --output=stepwise-f.%j.out
#SBATCH --error=stepwise-f.%j.err
#SBATCH --time=01:00:00
#SBATCH --partition=fm-bigmem-1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=40G

###Load necessary module
module load R/4.1.2

###Run script
Rscript stepwise-f.R

###Unload loaded modules
module unload R/4.1.2

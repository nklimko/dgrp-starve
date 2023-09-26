#!/bin/bash

#SBATCH --job-name=methodF
#SBATCH --output=method-f.%j.out
#SBATCH --error=method-f.%j.err
#SBATCH --time=24:00:00
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G

###Load necessary module
module load R/4.1.2

###Run script
Rscript methComp-f.R

###Unload loaded modules
module unload R/4.1.2

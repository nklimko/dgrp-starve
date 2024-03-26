#!/bin/bash

#SBATCH --job-name=minusM
#SBATCH --output=method-m.%j.out
#SBATCH --error=method-m.%j.err
#SBATCH --time=24:00:00
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G



###Load necessary module
module load R/4.1.2

###Set variables
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1

###Run script
Rscript minusM.R

###Unload loaded modules
module unload R/4.1.2

#!/bin/bash

#SBATCH --job-name=BGLRF
#SBATCH --output=out-bglr-f.%j.out
#SBATCH --error=err-bglr-f.%j.err
#SBATCH --time=24:00:00
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=56G

###Load necessary module
module load R/4.1.2

###Set variables
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1

###Run script
Rscript bglr-f.R

###Unload loaded modules
module unload R/4.1.2

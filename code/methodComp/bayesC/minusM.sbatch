#!/bin/bash

#SBATCH --job-name=bayesM
#SBATCH --output=method-m.%j.out
#SBATCH --error=method-m.%j.err
#SBATCH --time=24:00:00
#SBATCH --partition=fm-bigmem-1,fm-bigmem-2,fm-bigmem-3
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=20G

###Load necessary module
module load R/4.1.2

###Run script
Rscript bayesM.R

###Unload loaded modules
module unload R/4.1.2

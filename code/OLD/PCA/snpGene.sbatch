#!/bin/bash

#SBATCH --job-name=snpGene
#SBATCH --output=snpGene.%j.out
#SBATCH --error=snpGene.%j.err
#SBATCH --time=01:00:00
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G

###Load necessary module
module load R/4.0.3

###Run script
Rscript snpGene.R

###Unload loaded modules
module unload R/4.0.3

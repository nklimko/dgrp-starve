#!/bin/bash

#SBATCH --job-name=four-comp
#SBATCH --output=four-comp.%j.out
#SBATCH --error=four-comp.%j.err
#SBATCH --time=01:00:00
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G

###Load necessary module
module load R/4.0.3

###Run script
Rscript line_avgMinus.R
Rscript line_avgPlus.R
Rscript line_difMinus.R
Rscript line_difPlus.R

###Unload loaded modules
module unload R/4.0.3

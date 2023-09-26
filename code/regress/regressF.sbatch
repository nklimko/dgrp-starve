#!/bin/bash

#SBATCH --job-name=regressF
#SBATCH --output=regressF.%j.out
#SBATCH --error=regressF.%j.err
#SBATCH --time=01:00:00
#SBATCH --partition=compute
#SBATCH --nodes=3
#SBATCH --cpus-per-task=4
#SBATCH --mem=5G

###Load necessary module
module load R/4.0.3

###Run script
Rscript regressF.R

###Unload loaded modules
module unload R/4.0.3

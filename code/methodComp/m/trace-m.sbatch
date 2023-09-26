#!/bin/bash

#SBATCH --job-name=trace-m
#SBATCH --output=%j.trace-m.out
#SBATCH --error=%j.trace-m.err
#SBATCH --time=01:00:00
#SBATCH --partition=fm-bigmem-1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G

###Load necessary module
module load R/4.1.2

###Run script
Rscript trace-m.R

###Unload loaded modules
module unload R/4.1.2

#!/bin/bash
#
#SBATCH --job-name=cobra
#SBATCH --ntasks=1
#SBATCH --partition=compute
#SBATCH --time=99:00:00
#SBATCH --mem=6gb
#SBATCH --output=/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/logs/11_trial/%j

#Set output above to snake/logs/{foldername of choice}


#This file runs snakemake in safe mode "-n flag" to ensure that desired rules are rerun correctly. 

#Choose snakefile by uncommenting: start with datafile to process starvation

sfile="code/snakefiles/datafile.yaml"
#sfile="code/snakefiles/srfile.yaml"
#sfile="code/snakefiles/GOfile.yaml"
#sfile="code/snakefiles/sparsefile.yaml"
#sfile="code/snakefiles/genefile.yaml"
#sfile="code/snakefiles/figfile.yaml"

#set working directory
cd /data2/morgante_lab/nklimko/rep/dgrp-starve

#mkdir -p ./{log,logs_slurm}

#source conda/mamba and activate snakemake
source /data2/morgante_lab/nklimko/software/miniconda3/etc/profile.d/conda.sh
source /data2/morgante_lab/nklimko/software/mambaforge-pypy3/etc/profile.d/mamba.sh
mamba activate snakemake

module load R/4.1.2

#Set linear algebra to single thread
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1

#Include personal R packages
export R_LIBS=/data2/morgante_lab/nklimko/software/R/x86_64-pc-linux-gnu-library/4.1

#snakemake -s $sfile -q -n --dag | dot | display 

snakemake \
-s $sfile \
-n \
-q \
--profile snake/slurm \
--nolock \
--rerun-triggers mtime \
--resources jobweight=101

module unload R/4.1.2

mamba deactivate

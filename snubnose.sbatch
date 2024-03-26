#!/bin/bash
#
#SBATCH --job-name=cobra
#SBATCH --ntasks=1
#SBATCH --partition=compute
#SBATCH --time=99:00:00
#SBATCH --mem=6gb
#SBATCH --output=/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/logs/11_trial/%j
#SBATCH --mail-type=fail
#SBATCH --mail-user=nklimko@clemson.edu


#sfile="trimFile.yaml"
#sfile="GOfile.yaml"
sfile="srfile.yaml"
#sfile="f_file.yaml"



cd /data2/morgante_lab/nklimko/rep/dgrp-starve
#mkdir -p ./{log,logs_slurm}

#source /opt/ohpc/pub/Software/mamba-rocky/etc/profile.d/conda.sh

source /data2/morgante_lab/nklimko/software/miniconda3/etc/profile.d/conda.sh
source /data2/morgante_lab/nklimko/software/mambaforge-pypy3/etc/profile.d/mamba.sh

mamba activate snakemake

module load R/4.1.2

#Set LA to single thread
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1

#Include personal R packages
#export R_LIBS=/data2/morgante_lab/nklimko/software/R
export R_LIBS=/data2/morgante_lab/nklimko/software/R/x86_64-pc-linux-gnu-library/4.1

#--dag | display | dot
#-p -n \

snakemake \
-s $sfile \
-n \
-q \
--profile snake/slurm \
--nolock \
--rerun-triggers mtime \
--resources jobweight=101
#-q \

#--ignore-incomplete

#--rerun-incomplete
#--touch

#--configfile dgrp.yaml \
#--conda-frontend conda \
#--latency-wait 30 \
#--local-cores 8

module unload R/4.1.2

mamba deactivate

This repository contains code and data resources to accompany our research paper:

> Klimkowski Arango, N. & Morgante, F. (2024).
> Comparing statistical learning methods for complex trait prediction from gene expression.
> *bioRxiv* 2024.06.01.596951.
> https://doi.org/10.1101/2024.06.01.596951

# dgrp-starve

Welcome! A brief overview of this repository can be found below

## Important folders:

### code
- snakefiles subdirectory contains all pipeline snakefiles
- other directories contain scripts specific to pipeline

### snake
- this folder is the core for all pipelines
- data folder
- slurm folder contains config file specific to computing cluster
- log folder contains output/error messages from all rules

### output
- figures and csv tables for use in publication

## Job submission:

### snake.sbatch
- safe-mode debug tool for job submissions
- allows prior confirmatino that snakemake will run proper rules without overwriting existing files
- select snakefile within script to confirm pipeline

### smake.sbatch
- job submitter for pipeline
- select snakefile within script to confirm pipeline

## workflowr

A workflowr site providing additional detail can be found [here][].

[here]: https://nklimko.github.io/dgrp-starve/index.html

A [workflowr][] project.

[workflowr]: https://github.com/workflowr/workflowr

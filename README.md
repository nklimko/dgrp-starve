# dgrp-starve

A breakdown of my research itself can be found [here][].

[here]: https://nklimko.github.io/dgrp-starve/index.html

A [workflowr][] project.

[workflowr]: https://github.com/workflowr/workflowr


Welcome!

This repository serves as the backbone for my research project in complex trait prediction. Most code is in R with a growing demand for python through snakemake and other scripts.




## directories

# code
- contains all scripts for pipeline
- pipeline methods are in the 'method' subfolder
- all others are smaller data preparation scripts


# logs
- subdirectories are made for each rule in snakefile
- old contains unsorted logs prior to mid June 

# slurm
- snakemake profile to set default resources for overarching run groups

## files

# dgrp.yaml
- config file for most jobs
- accessed using {config[variable]} in snakefile

# Snakefile.yaml
- snakefile containing all rules for multiple paths
  - sr: starvation resistance
  - fm: female and male
  - top3: starvation resistance + top three correlated traits
    - cafe: capillary feeding
    - free.glycerol
    - free.glucose
- coordinates with dgrp.yaml for most variables
- .yaml extension configures layout in Rstudio

# snakemake-submitter.sbatch
- file used to configure snakemake jobs in slurm
- contains useful config specifications
  - option to change config file if needed, split params by path





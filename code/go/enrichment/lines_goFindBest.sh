#!/bin/bash
Rscript goFindBest.R --method bayes --sex f;
Rscript goFindBest.R --method bayes --sex m;
Rscript goFindBest.R --method blup --sex f;
Rscript goFindBest.R --method blup --sex m;

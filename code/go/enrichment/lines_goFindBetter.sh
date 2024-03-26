#!/bin/bash
Rscript goFindBetter.R --method bayesC --sex f --num 100;
Rscript goFindBetter.R --method bayesC --sex f --num 50;
Rscript goFindBetter.R --method bayesC --sex f --num 25;
Rscript goFindBetter.R --method bayesC --sex m --num 100;
Rscript goFindBetter.R --method bayesC --sex m --num 50;
Rscript goFindBetter.R --method bayesC --sex m --num 25;
Rscript goFindBetter.R --method blup --sex f --num 100;
Rscript goFindBetter.R --method blup --sex f --num 50;
Rscript goFindBetter.R --method blup --sex f --num 25;
Rscript goFindBetter.R --method blup --sex m --num 100;
Rscript goFindBetter.R --method blup --sex m --num 50;
Rscript goFindBetter.R --method blup --sex m --num 25;

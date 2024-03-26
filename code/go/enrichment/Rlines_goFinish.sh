#!/bin/bash
Rscript goFinish.R --method bayesC --sex f --num 100;
Rscript goFinish.R --method bayesC --sex f --num 50;
Rscript goFinish.R --method bayesC --sex f --num 25;
Rscript goFinish.R --method bayesC --sex m --num 100;
Rscript goFinish.R --method bayesC --sex m --num 50;
Rscript goFinish.R --method bayesC --sex m --num 25;
Rscript goFinish.R --method blup --sex f --num 100;
Rscript goFinish.R --method blup --sex f --num 50;
Rscript goFinish.R --method blup --sex f --num 25;
Rscript goFinish.R --method blup --sex m --num 100;
Rscript goFinish.R --method blup --sex m --num 50;
Rscript goFinish.R --method blup --sex m --num 25;

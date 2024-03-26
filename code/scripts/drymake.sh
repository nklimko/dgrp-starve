savePath='/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/logs/11_trial/'

function queuePrint {
   sbatch '/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/snubnose.sbatch' | grep -o -E '[0-9]+'
#echo temp
}

ticket=$(queuePrint)
savePath+=$ticket

export SN=$savePath

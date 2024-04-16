#!/bin/bash
#micky

terms="top5.txt"
#terms="termList"
#terms="../data/go/41_terms/termList"

while read e
do
searchLine="$e"

#echo "LOOP THROUGH"
#echo $searchLine

grep $e -C1 "/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/03_goDB/go-basic.obo"

done < "$terms"

#search alts:


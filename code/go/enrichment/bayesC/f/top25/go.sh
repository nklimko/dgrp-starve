#!/bin/bash
#micky

terms="pointList"
hardPath="/data2/morgante_lab/nklimko/rep/dgrp-starve/snake/data/03_goDB/go-basic.obo"
#terms="../data/go/41_terms/termList"

while read e
do
searchLine="$e"

#echo "LOOP THROUGH"
#echo $searchLine

#grep $e -C1 "../data/03_goDB/go-basic.obo"
#grep $e -C1 "../../../../../data/03_goDB/go-basic.obo"
grep $e -C1 $hardPath

done < "$terms"

#search alts:






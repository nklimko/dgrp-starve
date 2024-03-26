#!/bin/bash
#micky

terms="topHitsF.txt"
#terms="../data/go/41_terms/termList"

while read e
do
searchLine="$e"

#echo "LOOP THROUGH"
#echo $searchLine

#grep $e -C1 "../data/03_goDB/go-basic.obo"
grep $e -C1 "../../../../../data/03_goDB/go-basic.obo"

done < "$terms"

#search alts:






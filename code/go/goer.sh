#!/bin/bash
#micky

terms="data/go/41_terms/termList"

while read e
do

query = "names: "
query += "$e"

grep query -A1 "data/03_goDB/go-basic.obo"

done < "$terms"

#search alts:


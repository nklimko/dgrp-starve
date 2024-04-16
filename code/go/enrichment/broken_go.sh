#!/bin/bash
#running "bash commandflow.sh" from this folder specifically, paths are relative to current folder for terminal
#partMake.r was written from snakemake overhead for consistency

#grab all completed terms
#ls ../data/go/33_metric/sexf/rmax0.8/rgo0.01 > dataList


#move up for R script to use correct paths coded
#cd ..

#R script to read data, create plot, return list of top hits(hard coded)
#Rscript goPost/partMake.R

#descend
#cd goPost

#match all hits to database
bash go.sh > rawHits

#filter for main entries
grep 'id: ' rawHits -A1 > trimHits

#print results from file
echo "Top Correlated GO terms, ordered:"
cat trimHits

grep 'GO:' trimHits > termList

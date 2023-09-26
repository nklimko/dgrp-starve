


if(bioconductor_works){
take in a GO term
find all genes from go term

match go term genes to all genes index

save index hits to rds




The bioconducter package contains the genome wide annotation for Drosophila melanogaster. Annotation is primarily based on mapping using Entrez Gene identifiers. Install the Bioconducter annotation package. For more information about the bioconducter package go to: https://bioconductor.org/packages/release/data/annotation/html/org.Dm.eg.db.html.

source("https://bioconductor.org/biocLite.R")

#loads org.d_mel_???_database
biocLite("org.Dm.eg.db")


library(org.Dm.eg.db, verbose = FALSE, quietly = TRUE)

load(file="./annotation/fbSets2.Rdata")
Link GO terms to Entrez Gene ids with the bioconducter package. Information about the gene ontology project can be found here: http://www.geneontology.org/. Since our dataset consists of fb gene ids, the fb gene ids have to be linked to entrez gene ids in order to be linked to GO terms.

Link fb gene ids to Entrez Gene ids.

fb2eg <- org.Dm.egFLYBASE2EG
mapped_genes <- mappedkeys(fb2eg)
fb2eg <- as.list(fb2eg[mapped_genes])
str(fb2eg[1:5])


Link Entrez Gene ids to fb gene ids.

eg2fb <- org.Dm.egFLYBASE
mapped_genes <- mappedkeys(eg2fb)
eg2fb <- as.list(eg2fb[mapped_genes])
str(eg2fb[1:5])
## List of 5
##  $ 10178776: chr "FBgn0261702"
##  $ 10178777: chr "FBgn0262024"
##  $ 10178779: chr "FBgn0058354"
##  $ 10178780: chr "FBgn0053929"
##  $ 10178781: chr "FBgn0262141"


Link GO ids to Entrez Gene ids.

go2eg <- as.list(org.Dm.egGO2EG)
str(go2eg[1:5])
## List of 5
##  $ GO:0000001: Named chr [1:2] "36309" "53567"
##   ..- attr(*, "names")= chr [1:2] "IMP" "IMP"
##  $ GO:0000002: Named chr [1:8] "33567" "34307" "35236" "38046" ...
##   ..- attr(*, "names")= chr [1:8] "IBA" "IMP" "IMP" "IBA" ...
##  $ GO:0000003: Named chr [1:5] "31153" "31309" "36619" "36619" ...
##   ..- attr(*, "names")= chr [1:5] "IBA" "NAS" "IMP" "NAS" ...
##  $ GO:0000011: Named chr "34110"
##   ..- attr(*, "names")= chr "IBA"
##  $ GO:0000012: Named chr [1:5] "31451" "33530" "41872" "42322" ...
##   ..- attr(*, "names")= chr [1:5] "IEA" "IBA" "ISS" "IBA" ...


Link fb gene ids to GO ids.

go2fb <- lapply(go2eg,function(x){ 
  fb <- na.omit(unlist(eg2fb[x]))
  m <- match(fb,names(fbSets)) 
  fb <- fb[!is.na(m)]
  fb <- fb[!duplicated(fb)]
  return(fb)
})
str(go2fb[1:5], vec.len = 3)
## List of 5
##  $ GO:0000001: Named chr "FBgn0261618"
##   ..- attr(*, "names")= chr "53567"
##  $ GO:0000002: Named chr [1:7] "FBgn0031540" "FBgn0032154" "FBgn0040268" ...
##   ..- attr(*, "names")= chr [1:7] "33567" "34307" "35236" ...
##  $ GO:0000003: Named chr [1:4] "FBgn0023509" "FBgn0000479" "FBgn0003742" ...
##   ..- attr(*, "names")= chr [1:4] "31153" "31309" "36619" ...
##  $ GO:0000011: Named chr "FBgn0261064"
##   ..- attr(*, "names")= chr "34110"
##  $ GO:0000012: Named chr [1:4] "FBgn0026751" "FBgn0260817" "FBgn0026737" ...
##   ..- attr(*, "names")= chr [1:4] "31451" "33530" "41872" ...


Only include GO terms that consist of 5 or more fb genes. Save the resulting GO SNP sets.

go2fb <- go2fb[sapply(go2fb,length)>=5]
save(go2fb,file="./annotation/go2fb.Rdata")


Create GO sets: SNPs linked to GO terms.

goSets <- lapply(go2fb,function(x){ unique(unlist(fbSets[x])) })
goSets <- goSets[sapply(goSets,length)>199]
str(goSets[1:5])
## List of 5
##  $ GO:0000022: chr [1:315] "2R_2636861" "2R_2637023" "2R_2637293" "2R_2637711" ...
##  $ GO:0000027: chr [1:644] "X_1771396" "X_1772294" "X_1772337" "X_1772796" ...
##  $ GO:0000028: chr [1:283] "X_1376457" "X_1376829" "X_1376835" "X_9448856" ...
##  $ GO:0000045: chr [1:1602] "X_7208847" "X_7209453" "X_7209470" "X_7209504" ...
##  $ GO:0000060: chr [1:1816] "X_2056263" "X_2056399" "X_2056417" "X_2056810" ...
save(goSets,file="./annotation/goSets2.Rdata")



}







inPath <- 'data/sr/10_matched/f_starvation.Rds'
idPath <- 'data/sr/02_ids/f/ids_1.Rds'
outPath <- 'data/junk.Rds'


temp <- readRDS(inPath)


tempnames <- colnames(temp[,-1])
tempnames 


j


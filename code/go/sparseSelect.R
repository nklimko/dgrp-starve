#Select 35 random and top 15

set.seed(123)

temp <- sample(1:2500, 35)

topF <- c('GO.0045819.Rds',
          'GO.0033500.Rds',
          'GO.0055088.Rds',
          'GO.0042675.Rds',
          'GO.0042277.Rds',
          'GO.0008586.Rds',
          'GO.0016042.Rds',
          'GO.0007368.Rds',
          'GO.0007638.Rds',
          'GO.0006644.Rds',
          'GO.0010898.Rds',
          'GO.0046488.Rds',
          'GO.0034389.Rds',
          'GO.0017056.Rds',
          'GO.0006865.Rds')

topM <- c('GO.0035008.Rds',
          'GO.0140042.Rds',
          'GO.0007485.Rds',
          'GO.0005811.Rds',
          'GO.0045819.Rds',
          'GO.0042461.Rds',
          'GO.0033500.Rds',
          'GO.0016327.Rds',
          'GO.0035003.Rds',
          'GO.0045186.Rds',
          'GO.0006044.Rds',
          'GO.0040018.Rds',
          'GO.0042593.Rds',
          'GO.0004402.Rds',
          'GO.0001738.Rds')



allF <- list.files(path='data/go/03_goterms/sexf/')
randF <- sample(allF, 35)
finalF <- c(topF, randF)

filesF <- paste0('data/go/03_goterms/sexf/', finalF)



allM <- list.files(path='data/go/03_goterms/sexm/')
randM <- sample(allM, 35)
finalM <- c(topM, randM)

filesM <- paste0('data/go/03_goterms/sexm/', finalM)


#make directory that contains new directives
#
#
#copy (old file name new file location

length(unique(filesF))
length(unique(filesM))

finalList <- cbind(f=filesF, m=filesM)


saveRDS(finalList, 'data/go/finalList.Rds')

new.folderF <- 'data/go/03_goterms/randSparseF'
new.folderM <- 'data/go/03_goterms/randSparseM'

file.copy(filesF, new.folderF)
file.copy(filesM, new.folderM)



saveRDS(temp, 'data/rand.Rds')




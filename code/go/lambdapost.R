final <- readRDS('data/go/sparse/f/GO.0045819/1.Rds')
default <- readRDS('data/go/sparse/f/GO.0045819/default_1.Rds')

write(temp, file = 'temp.txt')

lambda100 <- readRDS('data/go/sparse/f/GO.0045819/default_1.Rds')
lambda5k <- readRDS('data/go/sparse/f/GO.0045819/1.Rds')


#lambda = 100
summary(lambda100$fit$lambda)

#lambda = 5000
summary(lambda5k$fit$lambda)


customL <- fit$lambda 


saveRDS(customL, file='customL.Rds')

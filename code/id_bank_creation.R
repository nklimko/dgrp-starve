getwd()


n <- 198
fold <- 5
iter <- 500

id_bank <- vector(mode='list', length=iter)

for(i in 1:iter)
{
  id_bank[[i]] <- sample(1:n, as.integer(n / fold))
}

saveRDS(id_bank, "data/id_bank.Rds")

ids <- readRDS("data/id_bank.Rds")

test_IDs <- ids[[i]]

W_train <- W[-test_IDs,]
W_test <- W[test_IDs,]
y_train <- y[-test_IDs]
y_test <- y[test_IDs]






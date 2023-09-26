# install.packages("qgg")
library(qgg)

set.seed(1)

###Simulate data

#row count - lines
n <- 100

#col count - genes
p <- 1000
p_effect <- 10 

X <- matrix(rnorm(n*p), nrow=n, ncol = p)
colnames(X) <- paste0("gene", 1:ncol(X))
rownames(X) <- paste0("line", 1:nrow(X))
b <- c(rnorm(p_effect, mean=6), rep(0, p-p_effect))

e <- rnorm(n)
y <- drop(X%*%b) + e

dim(X)
length(b)
length(e)
length(y)

class(X)
class(b)




###Create model for covariates to adjust for (only an intercept in our case)
mu <- rep(1, length(y))
names(mu) <- names(y)

###Compute transcriptomic relationship matrix (accounts for structure based on expression levels)
W <- scale(X)
TRM <- tcrossprod(W)/ncol(W)

###Fit mixed model

fit <- greml(y = y, X = mu, GRM = list(TRM), verbose = TRUE)

fit

stat <- glma(fit = fit, W = W)
head(stat)
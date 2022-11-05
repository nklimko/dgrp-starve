#stat guru


x <- starveDiff$fStarve
y <- starveDiff$mStarve

t.test(starve$f)
mu <- mean(x)
s <- sd(x)
n <- length(x)


shapiro.test(x)

shapiro.test(y)

mu
s
n
######################################
#
# Noah Klimkowski
# analysisSR.R
# 11/10/2022
#
# summary statistics of 
#
#######################################

#Libraries and setup
library(dplyr)
library(data.table)
library(tidyr)
starve <- tibble(read.csv("/data/morgante_lab/nklimko/rep/dgrp-starve/data/starve.csv")) %>% select(-X)
x <- starve$f
y <- starve$m


#plot parameters
par(mfrow=c(1,2))

#Female Histogram
hist(x, 
     col="red",
     border="black",
     prob=TRUE,
     xlab = "Starvation Resistance",
     main = "Female Lines")

lines(density(x),
      lwd = 2,
      col = "black")

#Male Histogram
hist(y, 
     col="blue",
     border="black",
     prob=TRUE,
     xlab = "Starvation Resistance",
     main = "Male Lines")

lines(density(y),
      lwd = 2,
      col = "black")

#Summary stats
cbind(summary(x), summary(y))


# Comparative Boxplot
boxplot(x,
        y,
        col = c("red","blue"),
        names=c("Female", "Male"),
        main="Sex Comparison of Starvation Resistance",
        xlab="Starvation Resistance",
        ylab="Sex",
        horizontal = TRUE)

# Scatter Plot
plot(x,
     y,
     col="black",
     xlab="Female Starvation Resistance",
     ylab="Male Starvation Resistance",
     abline(reg=lm(y~x), 
            col="purple"))
text(x = 85, y = 25,
     "y = 14.2347x + 0.5192",
     cex = 0.75)
text(x = 85, y = 22.5,
     "R=0.4693, p-value < 2.2e-16",
     cex = 0.75)

#Trendline parameter determination
summary(lm(y~x))

# QQ Plots
#plot parameters
par(mfrow=c(1,2))

qqnorm(x, main="Female Distribution")
qqline(x)
qqnorm(y, main="Male Distribution")
qqline(y)

#Shapiro-Wilk Normality Tests
Female_Starvation <- x
Male_Starvation <- y

shapiro.test(Female_Starvation)
shapiro.test(Male_Starvation)


### Group selection

#plot parameters
par(mfrow=c(1,2))

#Difference
hist(starve$dif, 
     col="purple",
     border="black",
     xlab = "Change in Starvation Resistance",
     main = "Difference of Female and Male Lines")

#Average
hist(starve$avg, 
     col="pink",
     border="black",
     xlab = "Average Starvation Resistance",
     main = "Avg of Female and Male Lines")

### Scatter Plot
plot(starve$avg,
     starve$dif,
     col="black",
     xlab="Line Average",
     ylab="Intersex Difference",
     abline(reg=lm(starve$dif~starve$avg), 
            col="purple"))
text(x = 72, y = -12.5,
     "y = 0.3258 - 2.3984",
     cex = 0.75)
text(x = 72, y = -15,
     "R=0.1293, p-value < 1.37e-7",
     cex = 0.75)

#Trendline parameter determination
summary(lm(starve$dif~starve$avg))

## The problem with tied observations
wilcox.test(c(-2, -2, 2, 2, 5, 5, 5), mu = 3)

##We can suppress this warning message, but it's not recommended in practice
?suppressWarnings
suppressWarnings(wilcox.test(c(-2,-2,2,2,5,5,5),mu = 3))

### R example
x2 <- c(5.6, 6.1, 6.3, 6.4, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0,8.0, 8.1, 8.1, 8.2, 8.4, 8.5, 8.7, 9.4, 14.3, 26.0)

## Plot the density
plot(density(x2))

suppressWarnings(wilcox.test(x2, mu=9, conf.int=TRUE))

binom.test(sum(x2>9),length(x2),alternative = "two.sided")

t.test(x2,alternative = "two.sided",mu=9)

boxplot(x2)
##Better looking plots using ggplots 
library(ggplot2)
qplot(y=x2, x= 1, geom = "boxplot")+geom_abline(slope=0,intercept = 9)

qplot(y=x2, x= 1, geom = "violin")+geom_abline(slope=0,intercept = 9)

## Empirical and True Distribution
set.seed(123)
par(mfrow=c(1,2))
x <- seq(-3,3,length.out = 100)
?ecdf

emp <- ecdf(rnorm(20))
plot(emp,verticals = TRUE,lwd = 2,main="n = 20")
lines(x,pnorm(x))
# abline(h=0.95,col="red")
# abline(v=1.64,col="red")

emp <- ecdf(rnorm(50))
plot(emp,verticals = TRUE,lwd = 2,main="n = 50")
lines(x,pnorm(x))
# abline(h=0.95,col="red")
# abline(v=1.64,col="red")

########## QQ Plots

### One sample
set.seed(12)
y <- rnorm(100)
qqnorm(y, ylim=c(-3,3), main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(y, distribution = qnorm)

### Two samples same 
z <- rnorm(100)
qqplot(y,z,main = "Two-sample Q-Q Plot",
       xlab = "Sample 1 Quantiles", ylab = "Sample 2 Quantiles")

### Two samples different
z <- rcauchy(100)
qqplot(y,z,main = "Two-sample Q-Q Plot",
       xlab = "Sample 1 Quantiles", ylab = "Sample 2 Quantiles")

par(mfrow=c(1,1))
hist(z, freq = F)
hist(y, freq = F, col = rgb(1,0,0,0.5), add = T)

boxplot(y,z)


## So far we have seen only visual checks.
## How do we actually test?
ks.test(rnorm(20),pnorm)

install.packages("nortest")
library(nortest)
lillie.test(rnorm(20))

shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))

par(mfrow=c(1,2))
n <- 1e3
x <- c(0, sort(runif(n)), 1)
y <- c(0, seq(1 / 2, n - 1 / 2) / n, 1)
plot(x, sqrt(n) * (y - x), type="l", xlab="t", ylab="B(t)", main = "n=1000")

n <- 1e4
x <- c(0, sort(runif(n)), 1)
y <- c(0, seq(1 / 2, n - 1 / 2) / n, 1)
plot(x, sqrt(n) * (y - x), type="l", xlab="t", ylab="B(t)", main = "n = 10000")


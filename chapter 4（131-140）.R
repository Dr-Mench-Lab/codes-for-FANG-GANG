chapter 4  Page 131-140

## ACTIVITY MODULE 2.3
x1 <- rnorm(150, mean = 100, sd = 15)
op <- par(mar = rep(0, 4))   
plot.new( )
par(mfrow=c(3,1))
hist(x1, freq = FALSE, breaks = 20)
points(density(x1), type = "l")
rug(x1)

library(vioplot)
vioplot(x1, horizontal=TRUE, col="gray")

boxplot(x1, horizontal=TRUE)


## ACTIVITY MODULE 2.4
par(mfrow=c(1,1))
qqnorm(x1)
qqline(x1)

library(ggplot2)
df <- data.frame(x1)
p <- ggplot(df, aes(sample = x1))
p <- p + stat_qq()
print(p)

## ACTIVITY MODULE 2.5
par(mfrow=c(1,1))
install.packages("car")
library(car)
qqPlot(x1, las = 1, id = list(n = 6, cex = 1), lwd = 1, main="QQ Plot")


## ACTIVITY MODULE 2.6
## TEST 1
x2 <- runif(150, min = 50, max = 150)
op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))
hist(x2, freq = FALSE, breaks = 20)
points(density(x2), type = "l")
rug(x2)

library(vioplot)
vioplot(x2, horizontal=TRUE, col="gray")

boxplot(x2, horizontal=TRUE)

par(mfrow=c(1,1))
qqPlot(x2, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")


## TEST 2
x3.temp <- rnorm(150, mean = 0, sd = 1)
x3 <- sign(x3.temp)*x3.temp^2 * 15 + 100
op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))
hist(x3, freq = FALSE, breaks = 20)
points(density(x3), type = "l")
rug(x3)

library(vioplot)
vioplot(x3, horizontal=TRUE, col="gray")

boxplot(x3, horizontal=TRUE)

par(mfrow=c(1,1))
qqPlot(x3, las = 1,id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")

## TEST 3
x4 <- rexp(150, rate = 1)
op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))
hist(x4, freq = FALSE, breaks = 20)
points(density(x4), type = "l")
rug(x4)

library(vioplot)
vioplot(x4, horizontal=TRUE, col="gray")

boxplot(x4, horizontal=TRUE)

par(mfrow=c(1,1))
qqPlot(x4, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")

## TEST 4
x5 <- 15 - rexp(150, rate = 0.5)
op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))

hist(x5, freq = FALSE, breaks = 20)
points(density(x5), type = "l")
rug(x5)

library(vioplot)
vioplot(x5, horizontal=TRUE, col="gray")

boxplot(x5, horizontal=TRUE)

par(mfrow=c(1,1))
qqPlot(x5, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")

## ACTIVIY MODULE 2.7
shapiro.test(x1)
install.packages("nortest")
library(nortest)
ad.test(x1)
cvm.test(x1)

shapiro.test(x2)
library(nortest)
ad.test(x2)
cvm.test(x2)

shapiro.test(x3)
library(nortest)
ad.test(x3)
cvm.test(x3)

shapiro.test(x4)
library(nortest)
ad.test(x4)
cvm.test(x4)

shapiro.test(x5)
library(nortest)
ad.test(x5)
cvm.test(x5)

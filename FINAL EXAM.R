chapter 4  Page 141-148

## FINAL EXAM: ACTIVITY MODULE 2.8

shapiro.test(sleep$d)
library(nortest)
ad.test(sleep$d)
cvm.test(sleep$d)

op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))
hist(sleep$d, freq = FALSE, breaks = 20)
points(density(sleep$d), type = "l")
rug(sleep$d)


library(vioplot)
vioplot(sleep$d, horizontal=TRUE, col="gray")

boxplot(sleep$d, horizontal=TRUE)

par(mfrow=c(1,1))
qqPlot(sleep$d, las = 1, id = list(n = 4, cex = 1), lwd = 1, main="QQ Plot")

## FINAL EXAM: ACTIVITY MODULE 2.9
library(ggplot2)
p1 <- ggplot(andro, aes(x = sex, y = level, fill=sex))
p1 <- p1 + geom_boxplot()
# add a "+" at the mean
p1 <- p1 + stat_summary(fun = mean, geom = "point", shape = 3, size = 2)
#pl <- p1 + coord_flip()
p1 <- p1 + labs(title = "Androstenedione Levels in Diabetics")
#print(p1)

## Men
shapiro.test(men)
library(nortest)
ad.test(men)
cvm.test(men)
## Women
shapiro.test(women)
library(nortest)
ad.test(women)
cvm.test(women)


p2 <- ggplot(andro, aes(x = level, fill=sex))
p2 <- p2 + geom_histogram(binwidth = 20, alpha = 0.5, position="identity")
p2 <- p2 + labs(title = "Androstenedione Levels in Diabetics")
#print(p2)

op <- par(mar = rep(3, 4))   
plot.new( )
library(gridExtra)
grid.arrange(p1, p2, ncol=1)

# QQ plot
par(mfrow=c(2,1))
qqPlot(men, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot, Men")
qqPlot(women, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot, Women")


n = 10
r = 5
norm.many <- data.frame(id = rep(seq(1:r^2), n)
                        , x = rnorm(r^2 * n)
)
library(ggplot2)
p <- ggplot(norm.many, aes(x = x))
p <- p + geom_histogram(binwidth = 0.4)
p <- p + facet_wrap(~ id, ncol = r)
p <- p + labs(title = "Twenty-five samples of size n=10 from a Normal (0,l) distribution")
print(p)


n = 30
r = 5
norm.many <- data.frame(id = rep(seq(1:r^2), n)
                        , x = rnorm(r^2 * n)
)
library(ggplot2)
p <- ggplot(norm.many, aes(x = x))
p <- p + geom_histogram(binwidth = 0.4)
p <- p + facet_wrap(~ id, ncol = r)
p <- p + labs(title = "Twenty-five samples of size n=30 from a Normal (0,l) distribution")
print(p)

## FINAL EXAM: ACTIVITY MODULE 2.10
c(mean(men), mean(women), sd(men), sd(women))
c(IQR(men), IQR(women), length(men), length(women))
bartlett.test(level ~ sex, data = andro)
library(car)
leveneTest(level ~ sex, data = andro)
fligner.test(level ~ sex, data = andro)

chapter 5  Page 154-180

## FINAL EXAM: ACTIVITY MODULE 2.11
fat <- read.table(text="
Row fat1 fat2 fat3 fat4
1 164 178 175 155
2 172 191 186 166
3 168 197 178 149
4 177 182 171 164
5 190 185 163 170
6 176 177 176 168
", header=TRUE)
fat


install.packages('reshape2')
library(reshape2)

fat.long <- melt(fat,
                 id.vars=c("Row"),
                 measure.vars = c("fat1", "fat2", "fat3", "fat4"),
                 variable.name = "type",
                 value.name = "amount"
)
fat.long

fat.wide <- dcast(fat.long, Row ~ type, value.var = "amount")
fat.wide

install.packages('plyr')
library(plyr)

fat.summary <- ddply(fat.long,
                     "type",
                     function(X){
                       data.frame( m = mean(X$amount),
                                   s = sd(X$amount),
                                   n = length(X$amount)
                       )
                     }
)
fat.summary$se <- fat.summary$s/sqrt(fat.summary$n)

fat.summary$ci.l <- fat.summary$m - qt(1-.05/2, df=fat.summary$n-1) * fat.summary$se
fat.summary$ci.u <- fat.summary$m + qt(1-.05/2, df=fat.summary$n-1) * fat.summary$se
fat.summary


library(ggplot2)
p <- ggplot(fat.long, aes(x = type, y = amount))

p <- p + geom_hline(yintercept = mean(fat.long$amount),
                    colour = "black" , linetype = "dashed" , size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)

p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 6,
                      aes(colour = type), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                      width = .2, aes(colour=type), alpha = 0.8)
p <- p + labs(title = "Doughnut fat absorption") + ylab("amount absorbed (g)")
print(p)


fit.f <- aov(amount ~ type, data = fat.long)
summary(fit.f)
fit.f

## FINAL EXAM: ACTIVITY MODULE 2.12
pairwise.t.test(fat.long$amount, fat.long$type,
                pool.sd = TRUE, p.adjust.method = "none")

pairwise.t.test (fat.long$amount, fat.long$type,
                 pool.sd = TRUE, p.adjust.method = "bonf")

glabella <- read.table(text="
Row cauc afam naaa
1   5.75  6.00  8.00
2   5.50  6.25  7.00
3   6.75  6.75  6.00
4   5.75  7.00  6.25
5   5.00  7.25  5.50
6   5.75  6.75  4.00
7   5.75  8.00  5.00
8   7.75  6.50  6.00
9   5.75  7.50  7.25
10  5.25  6.25  6.00
11  4.50  5.00  6.00
12  6.25  5.75  4.25
13  NA   5.00  4.75
14  NA   NA   6.00
", header=TRUE)

glabella.long <- melt(glabella,
                      id.vars=c("Row"),
                      variable.name = "pop",
                      value.name = "thickness",
                      # remove NAs
                      na.rm = TRUE
)

names(glabella.long) <- c("Row", "pop", "thickness")

library(ggplot2)
p <- ggplot(glabella.long, aes(x = pop, y = thickness))
p <- p + geom_hline(yintercept = mean(glabella.long$thickness),
                    colour = "black", linetype = "dashed", size = 0.3, alpha = 0.5)
p <- p + geom_boxplot(size = 0.75, alpha = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.5)
p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 6,
                      aes(colour=pop), alpha = 0.8)
p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                      width = .2, aes(colour=pop), alpha = 0.8)
p <- p + labs(title = "Glabella thickness") + ylab("thickness (mm)")
print(p)

glabella.summary <- ddply(glabella.long, "pop",
                          function(X) { data.frame( m = mean(X$thickness),
                                                    s = sd(X$thickness),
                                                    n = length(X$thickness) ) } )
glabella.summary

fit.g <- aov(thickness ~ pop, data = glabella.long)
summary(fit.g)
fit.g

pairwise.t.test(glabella.long$thickness, glabella.long$pop,
                pool.sd = TRUE, p.adjust.method = "bonf")

TukeyHSD(fit.f)

TukeyHSD(fit.g)

pairwise.t.test(fat.long$amount, fat.long$type,
                pool.sd = TRUE, p.adjust.method = "BH")

pairwise.t.test(glabella.long$thickness, glabella.long$pop,
                pool.sd = TRUE, p.adjust.method = "BH")


op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))

hist(fit.g$residuals, freq = FALSE, breaks = 20)
points(density(fit.g$residuals), type = "l")
rug(fit.g$residuals)

library(vioplot)
vioplot(fit.g$residuals, horizontal=TRUE, col="gray")
boxplot(fit.g$residuals, horizontal=TRUE)

par(mfrow=c(1,1))
library(car)
qqPlot(fit.g$residuals, las = 1, id = list(n = 8, cex = 1), lwd = 1, main="QQ Plot")

shapiro.test(fit.g$residuals)

install.packages('nortest',repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
library(nortest)
ad.test(fit.g$residuals)

cvm.test(fit.g$residuals)

bartlett.test(thickness ~ pop, data = glabella.long)

library(car)
leveneTest(thickness ~ pop, data = glabella.long)

fligner.test(thickness ~ pop, data = glabella.long)

## FINAL EXAM: ACTIVITY MODULE 2.14
chds <- read.csv("D:\\ADA1_notes_05-CHDS.csv")
chds$smoke <- rep(NA, nrow(chds));

chds[(chds$m_smok == 0), "smoke"] <- "0 cigs" ;

chds[(chds$m_smok > 0)&(chds$m_smok < 20),"smoke"] <- "1-19 cigs" ;

chds[(chds$m_smok >= 20),"smoke"] <- "20+ cigs";
chds$smoke <- factor(chds$smoke)

p1 <- ggplot(chds, aes(x = c_bwt))
p1 <- p1 + geom_histogram(binwidth = .4)
p1 <- p1 + facet_grid(smoke ~ .)
p1 <- p1 + labs(title = "Child birthweight vs maternal smoking") +
  xlab("child birthweight (lb)")
#print(p1)

p2 <- ggplot(chds, aes(x = c_bwt, fill=smoke))
p2 <- p2 + geom_histogram(binwidth = .4, alpha = 1/3, position="identity")
p2 <- p2 + labs(title = "Child birthweight vs maternal smoking") +
  xlab("child birthweight (lb)")
#print(p2)


install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, ncol=1)

library(ggplot2)
p <- ggplot(chds, aes(x = smoke, y = c_bwt))

p <- p + geom_hline(yintercept = mean(chds$c_bwt),
                    colour = "black" , linetype = "dashed" , size = 0.3, alpha = 0.5)

p <- p + geom_boxplot(size = 0.75, alpha = 0.5)

p <- p + geom_point(position = position_jitter(w = 0.05, h = 0), alpha = 0.2)

p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
                      aes(colour=smoke), alpha = 0.8)

p <- p + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                      width = .2, aes(colour=smoke), alpha = 0.8)
p <- p + labs(title = "Child birthweight vs maternal smoking") +
  ylab("child birthweight (lb)")
print(p)

install.packages("car")
library(car)
op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(1,3))
qqPlot(subset(chds, smoke == "0 cigs")$c_bwt, las = 1, lwd = 1, main="QQ Plot, 0 cigs")
qqPlot(subset(chds, smoke == "1-19 cigs")$c_bwt, las = 1, lwd = 1, main="QQ Plot, 1-19 cigs")
qqPlot(subset(chds, smoke == "20+ cigs" )$c_bwt, las = 1, lwd = 1, main="QQ Plot, 20+ cigs")

library(nortest)
shapiro.test(subset(chds, smoke == "0 cigs" )$c_bwt)

ad.test( subset(chds, smoke == "0 cigs" )$c_bwt)

cvm.test( subset(chds, smoke == "0 cigs" )$c_bwt)

shapiro.test(subset(chds, smoke == "1-19 cigs")$c_bwt)

ad.test( subset(chds, smoke == "1-19 cigs")$c_bwt)

cvm.test( subset(chds, smoke == "1-19 cigs")$c_bwt)

shapiro.test(subset(chds, smoke == "20+ cigs" )$c_bwt)

ad.test( subset(chds, smoke == "20+ cigs" )$c_bwt)

cvm.test( subset(chds, smoke == "20+ cigs" )$c_bwt)


fit.c <- aov(c_bwt ~ smoke, data = chds)

op <- par(mar = rep(3, 4))   
plot.new( )
par(mfrow=c(3,1))

hist(fit.c$residuals, freq = FALSE, breaks = 20)
points(density(fit.c$residuals), type = "l")
rug(fit.c$residuals)

library(vioplot)

vioplot(fit.c$residuals, horizontal=TRUE, col="gray")

boxplot(fit.c$residuals, horizontal=TRUE)

par(mfrow=c(1,1))
library(car)
qqPlot(fit.c$residuals, las = 1, id = list(n = 0, cex = 1), lwd = 1, main="QQ Plot")


shapiro.test(fit.c$residuals)

library(nortest)
ad.test(fit.c$residuals)

cvm.test(fit.c$residuals)


chds.summary <- ddply(chds, "smoke",
                      function(X) { data.frame( m = mean(X$c_bwt),
                                                s = sd(X$c_bwt),
                                                n = length(X$c_bwt) ) } )
chds.summary

bartlett.test(c_bwt ~ smoke, data = chds)

library(car)
leveneTest(c_bwt ~ smoke, data = chds)

fligner.test(c_bwt ~ smoke, data = chds)

summary(fit.c)

TukeyHSD(fit.c)

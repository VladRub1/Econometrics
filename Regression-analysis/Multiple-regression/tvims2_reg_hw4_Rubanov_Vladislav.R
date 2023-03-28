library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(memisc)
library(sandwich)
library(lmtest)

# задание 1

lab <- read_dta('hwdata.dta')
head(lab)

# почистим наши данные
labhw <- dplyr::select(lab, -c(province,uezd,serfperc1,redist))
labhw <- na.omit(labhw)

m1 <- lm(ch_schools_pc ~ afreq + nozemstvo + distance_moscow + goodsoil + lnurban + lnpopn + province_capital, data = labhw)
summary(m1)

# задание 2

ggpairs(labhw[,-c(1, 8)])

ggcorr(labhw[,-c(1, 8)], label = TRUE)
corrmatrix <- cor(labhw[,-c(1, 8)])
corrmatrix 

vif <- vif(m1)
vif

# задание 4
## пункт 1

ggplot(labhw, aes(afreq, distance_moscow)) + geom_point()
ggplot(labhw, aes(distance_moscow, lnpopn)) + geom_point()
ggplot(labhw, aes(goodsoil, lnurban)) + geom_point()
ggplot(labhw, aes(goodsoil, lnpopn)) + geom_point()

ggplot(labhw, aes(m1$fitted.values, afreq)) + geom_point()
ggplot(labhw, aes(m1$fitted.values, goodsoil)) + geom_point()
ggplot(labhw, aes(m1$fitted.values, lnurban)) + geom_point()
ggplot(labhw, aes(m1$fitted.values, lnpopn)) + geom_point()

ggplot(labhw, aes(afreq, m1$residuals^2)) + geom_point()
ggplot(labhw, aes(distance_moscow, m1$residuals^2)) + geom_point()
ggplot(labhw, aes(goodsoil, m1$residuals^2)) + geom_point()
ggplot(labhw, aes(lnurban, m1$residuals^2)) + geom_point()
ggplot(labhw, aes(lnpopn, m1$residuals^2)) + geom_point()

ggplot(labhw, aes(m1$fitted.values, m1$residuals^2)) + geom_point()

## пункт 2

bptest(m1)

## пункт 3

testdata = labhw
testdata$testres = m1$residuals

ggpairs(testdata[,-c(1:9)])

gqtest(m1, order.by = ~afreq, data = labhw, fraction = 0.2, alternative = "less")

gqtest(m1, order.by = ~distance_moscow, data = labhw, fraction = 0.2, alternative = "greater")
gqtest(m1, order.by = ~goodsoil, data = labhw, fraction = 0.2, alternative = "less")
gqtest(m1, order.by = ~lnpopn, data = labhw, fraction = 0.2, alternative = "less")
gqtest(m1, order.by = ~province_capital, data = labhw, fraction = 0.2, alternative = "less")
gqtest(m1, order.by = ~nozemstvo, data = labhw, fraction = 0.2, alternative = "greater")

## пункт 4

vcovHC(m1) # ковариацонная матрица с дисперсией оценок по гл. диагонали с учетом гетероскедастичности

summary(m1) # старая модель и старые значения
coeftest(m1, vcov=vcovHC(m1)) # новые значения с учетом гетероскедастичности

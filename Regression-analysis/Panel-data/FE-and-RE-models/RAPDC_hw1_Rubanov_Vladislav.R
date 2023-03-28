library(haven)
library(plm)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)
library(psych)
library(GGally)

panel<-read_dta("RAPDC_hw1.dta")

summary(panel)
describe(panel)


ggpairs(panel)


# Задание 1
pooled1 <- plm(lncrime~I(lnpolice-mean(lnpolice)) + 
                 I(lndensity-mean(lndensity)) + 
                 I(nonwhite-mean(nonwhite)), data=panel, model="pooling")
summary(pooled1)

# Задание 2
LSDV <- lm(lncrime~I(lnpolice-mean(lnpolice)) + 
             I(lndensity-mean(lndensity)) + 
             I(nonwhite-mean(nonwhite)) +
             factor(county), data=panel)
summary(LSDV)

# визуализация
panel$ypredicted <- LSDV$fitted

ggplot(panel, aes(x = lnpolice, y = ypredicted, color = factor(county)))+geom_smooth(method = lm)

# Задание 3
fe <- plm(lncrime~I(lnpolice-mean(lnpolice)) +
            I(lndensity-mean(lndensity)) +
            I(nonwhite-mean(nonwhite)), data = panel, index=c("county", "year"), effect = "individual", model="within")
summary(fe)

# небольшая выборка
panel[panel$county==1, 6]
panel[panel$county==17, 6]
panel[panel$county==131, 6]

# Задание 4, F-test
pFtest(fe, pooled)

# Задание 5
re <- plm(lncrime~I(lnpolice-mean(lnpolice)) +
            I(lndensity-mean(lndensity)) +
            I(nonwhite-mean(nonwhite)), data=panel, index=c("county", "year"), model="random")
summary(re)

# 5.1 Тест Бреуша-Пагана
plmtest(pooled, type=c("bp"))

# 5.2
# небольшая выборка
# Отсортируем массивы
dfsort1 <- panel[order(panel$lndensity), ]
dfsort2 <- panel[order(panel$lndensity, decreasing = TRUE), ]

# Выведем номера округов
A = unique(head(dfsort1, 57)['county'])
B = unique(head(dfsort2, 65)['county'])

# Отсортируем массивы по y
dfsort3 <- panel[order(panel$lncrime), ]
dfsort4 <- panel[order(panel$lncrime, decreasing = TRUE), ]

# Выведем номера округов
C = unique(head(dfsort3, 30)['county'])
D = unique(head(dfsort4, 35)['county'])

intersect(A, C)
intersect(B, D)

# min
A
C

# max
B
D

# 5.3
phtest(fe, re)

# Задание 6

LSDV_time <- lm(lncrime~I(lnpolice-mean(lnpolice)) + 
                  I(lndensity-mean(lndensity)) + 
                  I(nonwhite-mean(nonwhite)) +
                  factor(year), data = panel)
summary(LSDV_time)


fe_time <- plm(lncrime~I(lnpolice-mean(lnpolice)) + 
                 I(lndensity-mean(lndensity)) + 
                 I(nonwhite-mean(nonwhite)) +
                 factor(year), data = panel, index=c("county", "year"), effect = "time", model = "within")
summary(fe_time)

pFtest(fe_time, pooled)


fe_twoways1 <- plm(lncrime~I(lnpolice-mean(lnpolice)) + 
                    I(lndensity-mean(lndensity)) + 
                    I(nonwhite-mean(nonwhite)) +
                    factor(year), data = panel, index=c("county", "year"), effect = "twoways", model = "within")
summary(fe_twoways1)

fe_twoways2 <- plm(lncrime~I(lnpolice-mean(lnpolice)) + 
                    I(lndensity-mean(lndensity)) + 
                    I(nonwhite-mean(nonwhite)) +
                    factor(county), data = panel, index=c("county", "year"), effect = "twoways", model = "within")
summary(fe_twoways2) # same


#bptest(fe)
#coeftest(fe, vcov = vcovHC, type = "HC3")
#summary(fe)
#coeftest(fe_time, vcov = vcovHC, type = "HC3")

bptest(LSDV_time)

coeftest(LSDV_time, vcov = vcovHC, type = "HC3")
summary(LSDV_time)


# Задание 7 для time
y_pred <- LSDV_time$fitted 
panel1 <- data.frame(panel, y_pred) 

merged <- panel1 %>% group_by(year)%>% summarize(., cor(lncrime, y_pred))%>% merge(panel1, ., by="year")
head(merged)

merged$new <- ifelse(abs(merged$`cor(lncrime, y_pred)`)<0.3,1,0)
unique(merged$year[merged$new==1])
unique(merged$year[merged$new==0]) #ok


LSDV_time_2 <- lm(lncrime~I(lnpolice-mean(lnpolice)) + 
                    I(lndensity-mean(lndensity)) + 
                    I(nonwhite-mean(nonwhite)) +
                    factor(year), merged[merged$new == 0,])
coeftest(LSDV_time_2, vcov = vcovHC, type = "HC3") #nice


# Задание 8

# для одного
# получим значения условной вариации предиктора state_capacity
var <- summarize(group_by(panel, county), var(lnpolice))
var

write.csv(var, "my_df.csv") # скачаем данные для визуализации в питоне

# выведем несколько примеров
panel[panel$county==141, ]
panel[panel$county==185, ]

panel[panel$county==57, ]

# оценим набор моделей, чтобы получить оценки коэф-тов для каждой страны
a <- group_by(panel, county) %>%
  do(data.frame(beta = coef(lm(lncrime ~ lnpolice, data = .))[2]))
a

# запишем значения чистых оценок и условную вариацию
m <- as.data.frame(merge(a, var, by ="county"))
m

# посчитаем взвешенные коэффициенты
m$coef <- m$beta*(m$`var(lnpolice)`/sum(m$`var(lnpolice)`))
sum(m$coef)

sum(m$`var(lnpolice)`)
write.csv(m, "my_df2.csv") # скачаем данные для визуализации в питоне


# для множественного случая

reg_crime_dens = lm(lncrime~lndensity +
                      factor(county), data=panel)
y_clear = reg_crime_dens$residuals

reg_crime_pol = lm(lnpolice~lndensity +
                     factor(county), data=panel)
x_clear = reg_crime_pol$residuals


panel$y_clear = y_clear
panel$x_clear = x_clear

# считаем
var2 <- summarize(group_by(panel, county), var(x_clear))
var2

write.csv(var2, "my_df3.csv") # скачаем данные для визуализации в питоне

# оценим набор моделей, чтобы получить оценки коэф-тов для каждой страны
a <- group_by(panel, county) %>%
  do(data.frame(beta = coef(lm(y_clear ~ x_clear, data = .))[2]))
a

# запишем значения чистых оценок и условную вариацию
m <- as.data.frame(merge(a, var2, by ="county"))
m

# посчитаем взвешенные коэффициенты
m$coef <- m$beta*(m$`var(x_clear)`/sum(m$`var(x_clear)`))
sum(m$coef)

sum(m$`var(x_clear)`)
write.csv(m, "my_df4.csv") # скачаем данные для визуализации в питоне

# тест на совпадение
var1 <- summarize(group_by(panel, county), var(lnpolice))
a1 <- group_by(panel, county) %>%
  do(data.frame(beta1 = coef(lm(lncrime ~ lnpolice, data = .))[2]))
m1 <- as.data.frame(merge(a1, var1, by ="county"))
m1$coef1 <- m1$beta1*(m1$`var(lnpolice)`/sum(m1$`var(lnpolice)`))
head(m1)     # без контролей
sum(m1$coef1) # без контролей


reg_crime_dens = lm(lncrime~lndensity + factor(county), data=panel)
y_clear = reg_crime_dens$residuals
reg_crime_pol = lm(lnpolice~lndensity + factor(county), data=panel)
x_clear = reg_crime_pol$residuals
panel$y_clear = y_clear
panel$x_clear = x_clear
var2 <- summarize(group_by(panel, county), var(x_clear))
a2 <- group_by(panel, county) %>%
  do(data.frame(beta2 = coef(lm(y_clear ~ x_clear, data = .))[2]))
m2 <- as.data.frame(merge(a2, var2, by ="county"))
m2$coef2 <- m2$beta2*(m2$`var(x_clear)`/sum(m2$`var(x_clear)`))
head(m2)     # с контролями
sum(m2$coef2) # с контролями

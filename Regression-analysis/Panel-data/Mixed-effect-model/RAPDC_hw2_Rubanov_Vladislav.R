library(haven)
library(psych)
library(dplyr)
library(arm)
library(multilevel)
library(lattice)
library(ggplot2)
library(lmerTest)
library(influence.ME)
library(sjPlot)
library(glmmTMB)
library(GGally)
library(car)
library(plm)
library(plotly)


ME <- read_dta("RAPDC_lab3_2022.dta")

ME$state_id <- as.factor(ME$state)
ME <- na.omit(ME)

# посмотрим на данные описательные статистики
head(ME)
describe(ME)

ggplotly(ggpairs(ME[,-c(2,3,8)],ggplot2::aes(colour=as.factor(party)))) 

options(scipen = 999)

### Задание 1
model4.2.1 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = ME)
summary(model4.2.1)

# ranef(model4.2.1) 
dotplot(ranef(model4.2.1, condVar=TRUE))

# влиятельные наблюдения
state_unique <- unique(ME$state_id)
inf <- influence(model4.2.1, group = "state_id", data = ME)
cooks.distance.estex(inf, sort=TRUE)
plot(inf, which = "cook", xlab = "COOK'S MEASURE", cutoff = 4/length(state_unique))  
# AL, CA, KY, NJ

# удалим влиятельные штаты
subset1 = subset(ME, ME$state_id != 'NJ')
subset2 = subset(ME, ME$state_id != 'KY')
subset3 = subset(ME, ME$state_id != 'CA')
subset4 = subset(ME, ME$state_id != 'AL')
subset5 = subset(ME, ME$state_id != 'NJ' & ME$state_id != 'KY' & ME$state_id != 'CA' & ME$state_id != 'AL')

model4.2.1.1 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = subset1)
summary(model4.2.1.1)

model4.2.1.2 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = subset2)
summary(model4.2.1.2) 

model4.2.1.3 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = subset3)
summary(model4.2.1.3) 

model4.2.1.4 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = subset4)
summary(model4.2.1.4) 

model4.2.1.5 <- lmer(votepct ~ money + acres + party + (1 + party|state_id), REML = FALSE, data = subset5)
summary(model4.2.1.5) 


### задание 3
# сохраним предсказанные значения (fixed + random part)
pr_re <- predict(model4.2.1)
resid <- ME$votepct - pr_re

# сохраним случайные эффекты
raf <- ranef(model4.2.1)
raf_Intercept <- raf$state_id[,1]
raf_party <-raf$state_id[,2]

# H0 : the sample is normally distributed
shapiro.test(resid)
shapiro.test(rnorm(500, mean = 0, sd = 1))

shapiro.test(raf_Intercept) # их мало
shapiro.test(raf_party)

# H0 : the sample is not normally distributed
ks.test(unique(resid), "pnorm")
length(resid) - length(unique(resid))

ks.test(raf_Intercept, "pnorm")
ks.test(raf_party, "pnorm")


# визуализация
# qqplot
qqPlot(resid)
qqPlot(unique(resid))

qqPlot(raf_Intercept)
qqPlot(raf_party)

# гистограммы
hist(resid, col='steelblue', main='Residuals histogram', breaks=15, 
     xlab='Остатки модели "model4.2.1"')
hist(raf_Intercept, col='steelblue', main='RE (Intercept) histogram', breaks=8,
     xlab='Случайные эффекты на константу')
hist(raf_party, col='steelblue', main='RE (party) histogram', breaks=8,
     xlab='Случайные эффекты на пол. партию')

### задание 4
# преобразование
ME$log_votepct <- log1p(ME$votepct)
ME$log_money <- log1p(ME$money)
# меняем NaN на 0, т.к. они были близки к нему
ME$log_money[85] = 0
ME$log_money[208] = 0
# проверяем
ME$log_money[is.nan(ME$log_money)]

ME$log_acres <- log1p(ME$acres)

# посмотрим на распределение до логарифмирования
ggpairs(ME[,-c(1:4, 6:8)])
# посмотрим на распределение после логарифмирования
ggpairs(ME[,-c(1:3, 8:11)])

# тестируем модели
model5.0 <- lmer(votepct ~ money + party + party*money + (1 + party|state_id), REML = FALSE, data = ME)
summary(model5.0)

model5.1 <- lmer(votepct ~ money + party + party*money + acres + (1 + party|state_id), REML = FALSE, data = ME)
summary(model5.1)

# сравним
anova(model5.0, model5.1)

# добавим логарифмированные переменные
model5.2 <- lmer(log_votepct ~ log_money + party + party*log_money + log_acres + (1 + party|state_id), REML = FALSE, data = ME)
summary(model5.2)

dotplot(ranef(model5.1, condVar=TRUE))
dotplot(ranef(model5.2, condVar=TRUE))

# визуализация различий
# простой вариант линейной связи
ME %>% 
  mutate(pr_re = predict(model5.0), pr_fe = predict(model5.0, re.form = NA)) %>% 
  ggplot(aes(x=money, y=pr_re, group = party)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(money, pr_fe)) + facet_wrap(~party) +
  ggtitle("Различие взаимосвязи финансирования и \nголосования по партиям") +
  xlab("Размер финансирования (money)") + ylab("Случайный и фиксированный \nэффекты")

# добавление контрольной переменной
ME %>% 
  mutate(pr_re = predict(model5.1), pr_fe = predict(model5.1, re.form = NA)) %>% 
  ggplot(aes(x=money, y=pr_re, group = party)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(money, pr_fe)) + facet_wrap(~party) +
  ggtitle("Различие взаимосвязи финансирования и \nголосования по партиям") +
  xlab("Размер финансирования (money)") + 
  ylab("Случайный и фиксированный \nэффекты") +
  labs(caption = "0 - Демократическая партия\n1 - Республиканская партия")

# логарифмирование
ME %>% 
  mutate(pr_re = predict(model5.2), pr_fe = predict(model5.2, re.form = NA)) %>% 
  ggplot(aes(x=log_money, y=pr_re, group = party)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(log_money, pr_fe)) + facet_wrap(~party) +
  ggtitle("Различие взаимосвязи финансирования и \nголосования по партиям") +
  xlab("Логарифм размера финансирования (log_money)") + 
  ylab("Случайный и фиксированный \nэффекты") +
  labs(caption = "0 - Демократическая партия\n1 - Республиканская партия")


# простая визуализация связи
ggplot(ME, aes(x=money, y=votepct)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  facet_wrap("party") +
  ggtitle("Совместное распределение финансирования и \nголосования по партиям") +
  xlab("Размер финансирования (money)") + 
  ylab("Доля голосов (votepct)") +
  labs(caption = "0 - Демократическая партия\n1 - Республиканская партия")
 
# log
ggplot(ME, aes(x=log_money, y=log_votepct)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  facet_wrap("party") +
  ggtitle("Совместное распределение финансирования и \nголосования по партиям") +
  xlab("Логарифм размера финансирования (log_money)") + 
  ylab("Доля голосов (votepct)") +
  labs(caption = "0 - Демократическая партия\n1 - Республиканская партия")


### задание 5
## гипотеза о связи money и party
# LSDV
LSDV_1 <- lm(votepct~money + party + acres + 
               party*money +
               state_id, data = ME)
summary(LSDV_1)

# внутригруп. преобр.
fe_1 <- plm(votepct~money + party + acres +
              party*money, data = ME, index=c("state_id"), effect = "individual", model="within")
summary(fe_1)

# log
fe_1.2 <- plm(log_votepct~log_money + party + log_acres +
              party*log_money, data = ME, index=c("state_id"), effect = "individual", model="within")
summary(fe_1.2)

## гипотеза о связи party и acres
# LSDV
LSDV_2 <- lm(votepct~money + party + acres + 
               party*acres +
               state_id, data = ME)
summary(LSDV_2)

# внутригруп. преобр.
fe_2.1 <- plm(votepct~money + party + acres +
              party*acres, data = ME, index=c("state_id"), effect = "individual", model="within")
summary(fe_2.1)

# log
fe_2.2 <- plm(log_votepct~log_money + party + log_acres +
                party*log_acres, data = ME, index=c("state_id"), effect = "individual", model="within")
summary(fe_2.2)

# cluster
to_clust <- ME %>% dplyr::select(log_votepct, log_money, party, log_acres)

library(factoextra)
fviz_nbclust(to_clust, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(to_clust, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

library(NbClust)
res <- NbClust(to_clust, min.nc = 2, max.nc = 8, 
               method = "kmeans")
fviz_nbclust(res)

kmeans_clust <- kmeans(to_clust, 4)
to_clust$klust <- factor(kmeans_clust$cluster)

ME$k <- factor(kmeans_clust$cluster)


fe_2.3 <- plm(log_votepct~log_money + party + log_acres +
                party*log_acres, data = ME, index=c("k"), effect = "individual", model="within")
summary(fe_2.3)

subset1 = subset(ME, ME$k == 1)
subset2 = subset(ME, ME$k == 2)
subset3 = subset(ME, ME$k == 3)
subset4 = subset(ME, ME$k == 4)

unique(subset1$state)
unique(subset2$state)
unique(subset3$state)
unique(subset4$state)


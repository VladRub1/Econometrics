library(haven)
library(plm)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)
library(psych)

panel<-read_dta("RAPDC_lab1.dta")

### Задание 1
# получим значения условной вариации предиктора state_capacity
var <- summarize(group_by(panel, country), var(state_capacity))
var

write.csv(var, "my_df.csv") # скачаем данные для визуализации в питоне

# выведем несколько примеров
panel[panel$country=='Serbia', ]
panel[panel$country=='Tajikistan', ]
panel[panel$country=='Croatia', ]
panel[panel$country=='Georgia', ]

# описательные статистики
summary(panel)
describe(panel)

# выведем несколько примеров
panel[panel$country=='Czech Republic', ]
panel[panel$country=='Turkmenistan', ]
panel[panel$country=='Moldova', ]

# оценим набор моделей, чтобы получить оценки коэф-тов для каждой страны
a <- group_by(panel, country) %>%
  do(data.frame(beta = coef(lm(fh_polity ~ state_capacity, data = .))[2]))
a

# выведем несколько примеров
panel[panel$country=='Slovak Republic', ]
panel[panel$country=='Moldova', ]
panel[panel$country=='Belarus', ]

# запишем значения чистых оценок и условную вариацию
m <- as.data.frame(merge(a, var, by ="country"))
m

# посчитаем взвешенные коэффициенты
m$coef <- m$beta*(m$`var(state_capacity)`/sum(m$`var(state_capacity)`))
sum(m$coef)

sum(m$`var(state_capacity)`)
write.csv(m, "my_df2.csv") # скачаем данные для визуализации в питоне

# оценим LSDV-модель и сравним коэф-ты
LSDV_country <- lm(fh_polity~state_capacity+country, data = panel)
summary(LSDV_country)
sum(m$coef)

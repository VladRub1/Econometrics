library(psych)
library(haven)

data <- read_sav("WVS6_RU.sav")

# отберем интересующие переменные
data <- subset(data, select = c(V131:V139))
data <- na.omit(data)
head(data)

# опишем данные
describe(data)

corr <- cor(data, method = "spearman")
corr

library(GGally)
ggpairs(data)

# Критерий КМО
KMO(data)

# Критерий согнутого колена
VSS.scree(corr)

# Разведывательный этап
efa1 <- fa(r = corr, nfactors = 2, fm = "pa", rotate = "varimax")
efa1
print(efa1$loadings, cutoff=0.3)

# Доп. проверка
efa1 <- fa(r = corr, nfactors = 3, fm = "pa", rotate = "varimax")
efa1
print(efa1$loadings, cutoff=0.3)

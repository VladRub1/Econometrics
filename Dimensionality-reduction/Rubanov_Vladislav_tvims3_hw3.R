# Теор. часть
A <- matrix(c(9, 2, 2, 6), nrow = 2, byrow = TRUE)
eigen(A)

1/sqrt(5)
2/sqrt(5)

# 1 задание
pro <- read.csv("protests.csv")

library(tidyverse)
d <- pro %>% dplyr::select(country, year, duration, part2,
                           reaction, arrested, wounded)
d <- na.omit(d)

head(d)
str(d)
summary(d)

final <- d %>% group_by(country, year) %>%
  summarise(duration = sum(duration),
            part2 = sum(part2),
            reaction = sum(reaction),
            arrested = sum(arrested),
            wounded = sum(wounded))
final <- as.data.frame(final)

head(final)

m <- final %>% dplyr::select(duration, part2, reaction, 
                             arrested, wounded)

str(m)
summary(m)

library(GGally)
ggpairs(m)

# 2 задание
pca <- prcomp(m, center = TRUE, scale = TRUE)
pca

# 3 задание
library(ggbiplot)
ggbiplot(pca, scale=0) + ggtitle("Соотношение старых и новых осей")

# 4 задание
summary(pca)

plot(pca, type = "l", main = "Scree plot")

# 5 - бонус

final2 <- final %>% dplyr::select(country, duration, part2,
                           reaction, arrested, wounded)
final2$country <- ifelse(final2$country == 'france', 1, 
                         ifelse(final2$country == 'germany', 2, 
                                ifelse(final2$country == 'netherlands', 3, 4)))

pca2 <- prcomp(final2, center = TRUE, scale = TRUE)
pca2

ggbiplot(pca2, scale=0, groups = as.factor(final2$country), ellipse = T) + 
  ggtitle("Соотношение старых и новых осей (страна как фактор)")


# получилось плохо
d2 <- d %>% dplyr::select(country, duration, part2,
                          reaction, arrested, wounded)
d2$country <- ifelse(d2$country == 'france', 1, 
                     ifelse(d2$country == 'germany', 2, 
                            ifelse(d2$country == 'netherlands', 3, 4)))

pca3 <- prcomp(d2, center = TRUE, scale = TRUE)
pca3
summary(pca3)

ggbiplot(pca3, scale=0, groups = as.factor(d2$country), ellipse = T) + 
  ggtitle("Соотношение старых и новых осей (страна как фактор)")


final$fr <- ifelse(final$country == "france", 1, 0)
final$ge <- ifelse(final$country == "germany", 1, 0)
final$ne <- ifelse(final$country == "netherlands", 1, 0)
final$sw <- ifelse(final$country == "switzerland", 1, 0)
m <- final[, 3:11]
pca <- prcomp(m, center = TRUE, scale = TRUE)
pca

library(ggbiplot)
ggbiplot(pca, scale=0) + ggtitle("Соотношение старых и новых осей")

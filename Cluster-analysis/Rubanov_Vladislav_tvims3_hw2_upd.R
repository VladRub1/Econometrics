x <- c(0, 7, 6, 4)
y <- c(6, 2, 4, 1)

dat <- cbind.data.frame(x, y)

D <- dist(dat, method = 'maximum')
D

hc <- hclust(D, method = "complete")
plot(hc, ylab = 'Chebyshev distance', 
     main = 'Hierarchical cluster analysis', 
     sub = 'complete linkage',
     xlab = '')

### Практическая часть

#1

dat <- read.csv('cpds_2019.csv')
View(dat)
head(dat)
str(dat)
summary(dat)

#2

library(tidyverse)
to_clust <- dat %>% select(poco, gov_type, vturn, 
                           gov_left1, green1)
rownames(to_clust) <- dat$country

library(GGally)
ggpairs(to_clust)

#3

D <- dist(scale(to_clust), method = "euclidean")

hc <- hclust(D, method = "average")
plot(hc, main = "Average linkage method", cex = 0.7,
     ylab = 'Euclidean distance',
     xlab = '',
     sub = '')
rect.hclust(hc, k = 4, border = "red")


hc_ward <- hclust(D, method = "ward.D2")
plot(hc_ward, main = "Ward's method", cex = 0.7,
     ylab = 'Euclidean distance',
     xlab = '',
     sub = '')
rect.hclust(hc_ward, k = 4, border = "red")


clust <- cutree(hc_ward, k = 4)
to_clust$clust <- factor(clust)

#4

cluster01 <- to_clust %>% filter(clust == 1)
View(cluster01)

cluster02 <- to_clust %>% filter(clust == 2)
View(cluster02)

cluster03 <- to_clust %>% filter(clust == 3)
View(cluster03)

cluster04 <- to_clust %>% filter(clust == 4)
View(cluster04)


to_clust %>% group_by(clust) %>% tally

to_clust %>% group_by(clust) %>% summarise_at(vars(poco:green1), 
                                          .funs = median)

to_clust %>% group_by(clust) %>% summarise_at(vars(poco:green1), 
                                              .funs = mean)

to_clust %>% group_by(clust) %>% summarise_at(vars(poco:green1), 
                                              .funs = sd)

# 1 переменная

library(ggplot2)
ggplot(data = to_clust, aes(x = clust, y = vturn, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Voter turnout in election (%)")

ggplot(data = to_clust, aes(x = clust, y = gov_type, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Type of government")

ggplot(data = to_clust, aes(x = clust, y = gov_left1, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Cabinet posts of left parties")

ggplot(data = to_clust, aes(x = clust, y = green1, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Cabinet posts of green parties")

# 2 переменные

ggplot(data = to_clust, aes(x = gov_type, y = vturn, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Type of government", y = 'Voter turnout in election (%)')
# Portugal out

ggplot(data = to_clust, aes(x = gov_left1, y = vturn, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Cabinet posts of left parties", 
       y = 'Voter turnout in election (%)')

ggplot(data = to_clust, aes(x = gov_left1, y = gov_type, fill = clust)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Cabinet posts of left parties", 
       y = 'Type of government')

# диаграммы рассеяния

ggplot(data = to_clust, aes(x = poco, y = vturn, color = clust)) + 
  geom_point(size = 5) + 
  theme_bw() +
  labs(x = "Post-communist countries", 
       y = 'Voter turnout in election (%)')

ggplot(data = to_clust, aes(x = green1, y = vturn, color = clust)) + 
  geom_point(size = 5) + 
  theme_bw() +
  labs(x = "Cabinet posts of green parties", 
       y = 'Voter turnout in election (%)')

ggplot(data = to_clust, aes(x = gov_left1, y = green1, color = clust)) + 
  geom_point(size = 5) + 
  theme_bw() +
  labs(x = "Cabinet posts of left parties", 
       y = 'Cabinet posts of green parties')

ggplot(data = to_clust, aes(x = poco, y = gov_left1, color = clust)) + 
  geom_point(size = 5) + 
  theme_bw() +
  labs(x = "Post-communist countries", 
       y = 'Cabinet posts of left parties')

ggplot(data = to_clust, aes(x = vturn, y = gov_type, color = clust)) + 
  geom_point(size = 5) + 
  theme_bw() +
  labs(x = "Voter turnout in election (%)", 
       y = 'Type of government')

# трехмерные графики

mycolors <- c('#F8766D', '#00BA38', '#619CFF')
to_clust$color <- mycolors[as.numeric(to_clust$clust)]

library(rgl)
setupKnitr()
plot3d(
  x = to_clust$poco,
  y = to_clust$gov_left1,
  z = to_clust$green1,
  col = to_clust$color,
  type = 's',
  radius = 3,
  xlab="Post-communist countries",
  ylab="Cabinet posts of left parties",
  zlab="Cabinet posts of green parties")
rglwidget()

setupKnitr()
plot3d(
  x = to_clust$vturn,
  y = to_clust$gov_type,
  z = to_clust$poco,
  col = to_clust$color,
  type = 's',
  radius = 2,
  xlab="Voter turnout in election (%)",
  ylab="Type of government",
  zlab="Post-communist countries")
rglwidget()

setupKnitr()
plot3d(
  x = to_clust$vturn,
  y = to_clust$gov_type,
  z = to_clust$gov_left1,
  col = to_clust$color,
  type = 's',
  radius = 3,
  xlab="Voter turnout in election (%)",
  ylab="Type of government",
  zlab="Cabinet posts of left parties")
rglwidget()

# формальные тесты

kruskal.test(to_clust$vturn ~ to_clust$clust)
kruskal.test(to_clust$gov_type ~ to_clust$clust)
kruskal.test(to_clust$gov_left1 ~ to_clust$clust)
kruskal.test(to_clust$green1 ~ to_clust$clust)

tab_poco <- table(to_clust$clust, to_clust$poco)
tab_poco
chisq.test(tab_poco)

test <- fisher.test(tab_poco)
test

?fisher.test

# 5
library(factoextra)
fviz_nbclust(to_clust[1:5], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(to_clust[1:5], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

library(NbClust)
res <- NbClust(to_clust[1:5], min.nc = 2, max.nc = 8, 
               method = "kmeans")
fviz_nbclust(res)

# 6

to_clust$color <- NULL

kclust <- kmeans(to_clust[1:5], 4)
to_clust$k <- factor(kclust$cluster)

to_clust %>% group_by(clust) %>% summarise_at(vars(poco:green1), 
                                          .funs = c(mean))

to_clust %>% group_by(k) %>% summarise_at(vars(poco:green1), 
                                       .funs = c(mean))


to_clust %>% group_by(clust) %>% summarise_at(vars(poco:green1), 
                                              .funs = c(median))

to_clust %>% group_by(k) %>% summarise_at(vars(poco:green1), 
                                          .funs = c(median))


cluster11 <- to_clust %>% filter(k == 1)
View(cluster11)

cluster12 <- to_clust %>% filter(k == 2)
View(cluster12)

cluster13 <- to_clust %>% filter(k == 3)
View(cluster13)

cluster14 <- to_clust %>% filter(k == 4)
View(cluster14)



library(fossil)

rand.index(as.integer(to_clust$clust), 
           as.integer(to_clust$k))

install.packages("haven")
install.packages("psych")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("car")
library(haven)
library(psych)
library(ggplot2)
library(dplyr)
library(car)

######Задание 1#####

# пункт 1

marks <- read.table("reg_hw.txt",
           header = TRUE,
           sep = ";")

marks
group <- rep(1:4, c(25,35,35,25))
data <- data.frame(marks, group)

data <- transform(data, group=LETTERS[group])

data

# пункт 2

group_by(data, group) %>% summarise(mean = mean(micro), var = var(micro), IQR = IQR(micro), median = median (micro),min = min (micro), max = max (micro))

summary(data$micro)
var(data$micro)
IQR(data$micro)

leveneTest(micro ~ as.factor(group), data)

summary(data)

###

boxplot(data$micro, col = "aquamarine")

boxplot(data$micro ~data$group, xlab="Group", ylab="Grades", col = "coral")

data %>%
  ggplot(aes(micro, fill=as.factor(group),color="inferno"))+
  geom_histogram(binwidth = 1)+
  theme(axis.text.x = element_text(angle=90))+
  theme(panel.background = element_rect(fill="gray12",colour="gray12")) +
  theme(plot.background = element_rect(fill = "gray12"))+
  theme(panel.grid.minor = element_line(color = 'cyan1'))+
  theme(panel.grid.major = element_line(color = 'cyan1'))+
  theme(plot.title = element_text(colour = "chartreuse1",size=14))+
  theme(axis.title.x = element_text(colour = "chartreuse1",size=16, vjust=0.5),axis.title.y = element_text(colour = "chartreuse1",size=16))+
  theme(axis.text.x = element_text( colour ="cyan1",size=14))+
  theme(axis.text.y = element_text( colour ="cyan1",size=10))

# пункт 3

summary(aov(micro ~ as.factor(group), data))

pf(0.677, 3, 116, lower.tail = F)

oneway.test(micro ~ as.factor(group), data)


#####Задание 2#####

# пункт 1

lab1 <- dplyr::select(data, math, micro)
describe(lab1)

m1 <- lm(micro ~ math, data = lab1)
summary(m1)

# пункт 2

m1$coefficients

# пункт 3

pt(6.236, df = 120 - 2, lower.tail = F)*2

pt(3.950, df = 120 - 2, lower.tail = F)*2

# пункт 4

m1$coefficients[1] - qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[1]
m1$coefficients[1] + qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[1]

m1$coefficients[2] - qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[2]
m1$coefficients[2] + qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[2]

confint(m1)

# пункт 6

anova(m1)

pf(15.606, 1, 118, lower.tail = F)

# пункт 7 (доп.)

ggplot(data = lab1, aes(x = math, y = micro)) +
  geom_smooth(method="lm",se=F) +
  geom_point()

library(plotly)
d<-ggplot(data = lab1, aes(x = math, y = micro)) +
  geom_smooth(method="lm",se=F) +
  geom_point()

ggplotly(d)

cor.test(lab1$math, lab1$micro)
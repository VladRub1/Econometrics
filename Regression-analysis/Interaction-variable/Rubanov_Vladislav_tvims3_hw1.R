library(foreign) 
library(dplyr)
library(ggplot2)
library(GGally)
library(margins)
library(psych)
library(memisc)

df <- read.dta("tvims3_hw1.dta")

### задание 1

labhw <- df %>% dplyr::select(country, cpi, dem, fp, loggdppc, stab, britcol) %>% na.omit() 

labhw <- mutate(labhw, cpi = 10 - cpi)

head(labhw)
describe(labhw)
summary(labhw)

ggpairs(labhw[,-c(1)])

### задание 2

# создание подгрупп

test_data <- mutate(labhw, freedom_of_press = as.numeric(labhw$fp > 70))
test_data$freedom_of_press <- dplyr::recode(test_data$freedom_of_press, "1" = "free", "0" = "not free")
test_data <- mutate(test_data, freedom_of_press = factor(freedom_of_press))

head(test_data)

# график

ggplot(data = test_data, aes(x = dem, y = cpi, color = freedom_of_press)) +
  geom_point(size=2) + labs(x = "Level of democracy", 
                            y = "Level of corruption",
                            title = "The effect of corruption on democracy") + 
  geom_smooth(method=lm)+ scale_colour_manual(values = c("coral3", "darkorchid3"))

# модели

free <- subset(test_data, freedom_of_press == 'free')
m1_1 <- lm(cpi ~ dem, data = free)
summary(m1_1)

not_free <- subset(test_data, freedom_of_press == 'not free')
m1_2 <- lm(cpi ~ dem, data = not_free)
summary(m1_2)

mtable(m1_1, m1_2)

### задание 3

m2 <- lm(cpi ~ dem + fp + dem:fp, data = labhw)
summary(m2)

#центрирование

labhw_cen <- mutate(labhw, dem_c = dem - mean(dem))
labhw_cen$dem <- NULL

head(labhw_cen)

m3 <- lm(cpi ~ dem_c + fp + dem_c:fp, data = labhw_cen)
summary(m3)

### задание 4

# предельный эффект

margins_m3 <- margins(m3)
summary(margins_m3)
plot(margins_m3)

# значения fp от 0 до 100 с шагом в 10

s <- margins(m3, at = list(fp = seq(0, 100, by = 10)))
summary(s)

# график ME

ggplot(s, aes(x = fp)) + 
  geom_line(aes(y = dydx_dem_c)) +
  geom_line(aes(y = dydx_dem_c+1.96*sqrt(Var_dydx_dem_c)), linetype = 2) +
  geom_line(aes(y = dydx_dem_c-1.96*sqrt(Var_dydx_dem_c)), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Conditional effect") +
  xlab("Freedom of press (fp)") + ylab("Marginal effect of democracy")

# смотрим, где ME незначимый

s2 <- margins(m3, at = list(fp = seq(45, 60, by = 0.5)))
summary(s2)

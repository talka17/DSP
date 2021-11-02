library(tidyverse)
library(magrittr)
library(gmodels)

#reading dataset
project <- read_csv("C:\\Users\\sedov\\Documents\\archive\\dataset.csv")

fit <- lm(Deaths ~ Obesity, data = project)
summary(fit)

#30 tibbles ktore tvoria 40% dat z nasho datasetu
data <- map(1:30, ~ project[sort(sample(1:dim(project)[1], size = 0.4*dim(project)[1])),])
models <- map(data, ~ lm(.x$Deaths ~ .x$Obesity))

listOfFuntions <- list(coefficients = coef, residuals = residuals)
f <- function(x) {sapply(listOfFuntions, function(g) g(x))}
extractedData <- map(models, ~f(.x))

sd(map_dbl(models, ~ coef(.x)[1]))
sd(map_dbl(models, ~ coef(.x)[2]))

rss <- map_dbl(models, ~ sum(resid(.x)^2))
rse <- map_dbl(rss, ~ sqrt(.x/0.4*dim(project)[1]-2))
boxplot(rss)
boxplot(rse)

cfs <- map_dbl(models, ~ coef(.x)[2])
t.test(cfs, mu=0)



# pridane Tamara
dataset <- project

# zistenie vsetkych premennych obsiahnutych v datasete
dataset %>% colnames()

# vybrane premenne pre hypotezu 1
hypoteza1 <- dataset %>% select(Country, Obesity, Population, Deaths) %>% drop_na()
hypoteza1 %>% summary()
hypoteza1 %>% arrange(Obesity)
hypoteza1 %>% arrange(desc(Obesity))
hypoteza1 %>% arrange(Deaths)
hypoteza1 %>% arrange(desc(Deaths))

# grafy
ggplot(data = hypoteza1, mapping = aes(x = Country, y = Obesity))+
  geom_point(color = "red")

ggplot(data = hypoteza1, mapping = aes(x = Country, y = Deaths))+
  geom_point(color = "blue")

# alternativa/ reprezentovat to cez boxplot
hypoteza1 %>% select(Obesity) %>% boxplot()
hypoteza1 %>% select(Deaths) %>% boxplot()

# identifikovanie NA hodnot pre obezitu a umrtia
dataset %>% select(Country, Obesity) %>% filter(is.na(Obesity))
dataset %>% select(Country, Deaths) %>% filter(is.na(Deaths))


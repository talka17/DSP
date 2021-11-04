#hypoteza 2

library(tidyverse)
library(magrittr)
library(gmodels)

project1 <- read_csv("C:\\Users\\sedov\\Documents\\archive\\dataset.csv")

project <- project1 %>% select(Country, 'Fruits - Excluding Wine', Vegetables,+
  Population, Recovered) %>% drop_na()

names(project)[2] <- 'Fruits'
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>% 
  select(Country, Fruits, Vegetables, Population, Recovered, FruitsAndVegetables) %>% 
  drop_na()

fit <- lm(Recovered ~ FruitsAndVegetables, data = ourData)
summary(fit)

data <- map(1:30, ~ ourData[sort(sample(1:dim(ourData)[1], size = 0.4*dim(ourData)[1])),])
models <- map(data, ~ lm(.x$Recovered ~ .x$FruitsAndVegetables))

listOfFuntions <- list(coefficients = coef, residuals = residuals)
f <- function(x) {sapply(listOfFuntions, function(g) g(x))}
extractedData <- map(models, ~f(.x))

sd(map_dbl(models, ~ coef(.x)[1]))
sd(map_dbl(models, ~ coef(.x)[2]))

rss <- map_dbl(models, ~ sum(resid(.x)^2))
rse <- map_dbl(rss, ~ sqrt(.x/0.4*dim(ourData)[1]-2))
boxplot(rss)
boxplot(rse)

cfs <- map_dbl(models, ~ coef(.x)[2])
t.test(cfs, mu=0)
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
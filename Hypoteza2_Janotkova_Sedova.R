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

# vyber len koeficientov beta0 zo vsetkych lin. modelov
beta0_all <- map_dbl(models, ~ coef(.x)[1])
# vypocet standardnej odchylky pre koeficient beta0=intercept v ramci vsetkych podmnozin
beta0_sd <- sd(beta0_all)
beta0_all # z vypisu pozeram, ake boli ziskane hodnoty a ako sa menili

# vyber len koeficientov beta1 zo vsetkych lin. modelov
beta1_all <- map_dbl(models, ~ coef(.x)[2])
# vypocet standardnej odchylky pre koeficient beta1=slope v ramci vsetkych podmnozin
beta1_sd <- sd(beta1_all)
beta1_all

rss <- map_dbl(models, ~ sum(resid(.x)^2))
rse <- map_dbl(rss, ~ sqrt(.x/0.4*dim(ourData)[1]-2))
boxplot(rss)
boxplot(rse)

cfs <- map_dbl(models, ~ coef(.x)[2])
t.test(cfs, mu=0)

barplot(height=ourData$FruitsAndVegetables, names=ourData$Country,col="#69b3a2", las=2)

#zistovanie NA
project1 <- read_csv("C:\\Users\\sedov\\Documents\\archive\\dataset.csv")
names(project1)[9] <- 'Fruits'
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>%
  select(Country, Fruits, Vegetables, Population, Recovered, FruitsAndVegetables)

ourData %>% select(Country, FruitsAndVegetables) %>% filter(is.na(FruitsAndVegetables))
ourData %>% select(Country, Recovered) %>% filter(is.na(Recovered))

# vybrane premenne pre hypotezu 2
project1 <- read_csv("C:\\Users\\sedov\\Documents\\archive\\dataset.csv")

project <- project1 %>% select(Country, 'Fruits - Excluding Wine', Vegetables,+
  Population, Recovered) %>% drop_na()

names(project)[2] <- 'Fruits'
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>% 
  select(Country, Fruits, Vegetables, Population, Recovered, FruitsAndVegetables) %>% 
  drop_na()
ourData %>% summary()
ourData %>% arrange(FruitsAndVegetables)
ourData %>% arrange(desc(FruitsAndVegetables))
ourData %>% arrange(Recovered)
ourData %>% arrange(desc(Recovered))

#linearny model
project1 <- read_csv("C:\\Users\\sedov\\Documents\\archive\\dataset.csv")

project <- project1 %>% select(Country, 'Fruits - Excluding Wine', Vegetables,+
                                 Population, Recovered) %>% drop_na()

names(project)[2] <- 'Fruits'
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>% 
  select(Country, Fruits, Vegetables, Population, Recovered, FruitsAndVegetables) %>% 
  drop_na()

fit <- lm(Recovered ~ FruitsAndVegetables, data = ourData)
summary(fit)

# vyber koeficientov
beta0 = coef(fit)[1]
beta1 = coef(fit)[2]
# pomocna funkcia
lin_fun <- function(x){beta0 + beta1*x}
# vykreslenie lm aplikovaneho na cely dataset bez regresie
ggplot(ourData, aes(FruitsAndVegetables, Recovered)) + geom_point(colour = 'blue') + geom_function(fun = lin_fun, colour = 'red')


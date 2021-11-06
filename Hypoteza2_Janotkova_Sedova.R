#hypoteza 2

library(tidyverse)
library(magrittr)
library(gmodels)

#precitanie dat zo suboru 
project1 <- read_csv("dataset.csv")

#-----PREDSTAVENIE DAT
#premenovanie stlpca Fruits - Excluding Wines na Fruits
names(project1)[9] <- 'Fruits'

#zlucenie stlpcov Fruits a Vegetables
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>%
  select(Country, Fruits, Vegetables, Population, Recovered, FruitsAndVegetables)

#identifikovanie NA merani pre premenne FruitsAndVegetables a Recovered
ourData %>% select(Country, FruitsAndVegetables) %>% filter(is.na(FruitsAndVegetables))
ourData %>% select(Country, Recovered) %>% filter(is.na(Recovered))

#vyber premennych pre hypotezu 2
project <- project1 %>% select(Country, 'Fruits - Excluding Wine', Vegetables, Population, Recovered) %>% drop_na()
names(project)[2] <- 'Fruits'
ourData <- project %>% mutate(FruitsAndVegetables = (Fruits+Vegetables)/2) %>% 
  select(Country, Population, Recovered, FruitsAndVegetables) %>% 
  drop_na()

#vlastnosti pre kazdy stlpec
ourData %>% summary()

#zoradenie merani podla zvolenej premennej zostupne/vzostupne
ourData %>% arrange(FruitsAndVegetables)
ourData %>% arrange(desc(FruitsAndVegetables))
ourData %>% arrange(Recovered)
ourData %>% arrange(desc(Recovered))

# vizualizacia merani premennnej FruitsAndVegetables
#45x20inches, portrait = exportovat a vyzera to ok 
barplot(height=ourData$FruitsAndVegetables, names=ourData$Country,col="#69b3a2", las=2)


#-----HLADANIE LINEARNEHO MODELU PRE ZVOLENE DATA
#linearny model pre cele data
fit <- lm(Recovered ~ FruitsAndVegetables, data = ourData)
summary(fit)

#vyber koeficientov
beta0 = coef(fit)[1]
beta1 = coef(fit)[2]
#pomocna funkcia
lin_fun <- function(x){beta0 + beta1*x}
# vykreslenie lm aplikovaneho na cely dataset bez regresie
ggplot(ourData, aes(FruitsAndVegetables, Recovered)) + geom_point(colour = 'blue') + geom_function(fun = lin_fun, colour = 'red')

#30 tibbles, ktore tvoria 40% nahodne zvolenych merani z nasho datasetu
data <- map(1:30, ~ ourData[sort(sample(1:dim(ourData)[1], size = 0.4*dim(ourData)[1])),])
models <- map(data, ~ lm(.x$Recovered ~ .x$FruitsAndVegetables))

listOfFuntions <- list(coefficients = coef, residuals = residuals)
f <- function(x) {sapply(listOfFuntions, function(g) g(x))}
extractedData <- map(models, ~f(.x))

#vyber len koeficientov beta0 zo vsetkych lin. modelov
beta0_all <- map_dbl(models, ~ coef(.x)[1])
#vypocet standardnej odchylky pre koeficient beta0=intercept v ramci vsetkych podmnozin
beta0_sd <- sd(beta0_all)
beta0_all # z vypisu pozeram, ake boli ziskane hodnoty a ako sa menili

#vyber len koeficientov beta1 zo vsetkych lin. modelov
beta1_all <- map_dbl(models, ~ coef(.x)[2])
#vypocet standardnej odchylky pre koeficient beta1=slope v ramci vsetkych podmnozin
beta1_sd <- sd(beta1_all)
beta1_all

#vypocty a vizualizacie RSS a RSE-> ako dobre model sedi danym datam
rss <- map_dbl(models, ~ sum(resid(.x)^2))
boxplot(rss)
rse <- map_dbl(rss, ~ sqrt(.x/0.4*dim(ourData)[1]-2))
boxplot(rse)

#test, ci existuje zavislost medzi premennymi- ci beta1 je vzdy rozna od 0
t.test(beta1_all, mu=0)
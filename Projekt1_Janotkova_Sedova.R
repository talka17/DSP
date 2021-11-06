library(tidyverse)
library(magrittr)
library(gmodels)

# precitanie pomocou read_csv z tidyverse, ktora csv ulozi do tibble
dataset <- read_csv("dataset.csv")




#-----PREDSTAVENIE DAT
# zistenie vsetkych premennych obsiahnutych v datasete
dataset %>% colnames()

# identifikovanie NA merani pre premenne obezita a umrtia
dataset %>% select(Country, Obesity) %>% filter(is.na(Obesity))
dataset %>% select(Country, Deaths) %>% filter(is.na(Deaths))

# selekcia premennych pre hypotezu 1
hypoteza1 <- dataset %>% select(Country, Obesity, Population, Deaths) %>% drop_na()
# pre kazdy stlpec tibblu vyjadri jeho vlastnosti
hypoteza1 %>% summary()

# zoradenie merani podla zvolenej premennej od min po max
hypoteza1 %>% arrange(Obesity)
hypoteza1 %>% arrange(Deaths)

# zoradenie merani podla zvolenej premennej zostupne
hypoteza1 %>% arrange(desc(Obesity))
hypoteza1 %>% arrange(desc(Deaths))

# vizualizacia merani premennnej obezita
#45x20inches, portrait = exportovat a vyzera to ok 
barplot(height=hypoteza1$Obesity, names=hypoteza1$Country,col="#69b3a2", las=2)




#-----HLADANIE LINEARNEHO MODELU PRE ZVOLENE DATA

# linearny model pre cele data
fit <- lm(Deaths ~ Obesity, data = hypoteza1)
# vypise zistene informacie o objekte
summary(fit)

# vyber koeficientov
beta0 = coef(fit)[1]
beta1 = coef(fit)[2]
# pomocna funkcia
lin_fun <- function(x){beta0 + beta1*x}
# vykreslenie lm aplikovaneho na cely dataset bez regresie
ggplot(hypoteza1, aes(Obesity, Deaths)) + geom_point(colour = 'blue') + geom_function(fun = lin_fun, colour = 'red')


#60 tibbles ktore tvoria 40% nahodne zvolenych merani z nasho datasetu
data <- map(1:60, ~ hypoteza1[sort(sample(1:dim(hypoteza1)[1], size = 0.4*dim(hypoteza1)[1])),])
# zistovanie lin. modelov pre kazdy podmnozinu datasetu
models <- map(data, ~ lm(.x$Deaths ~ .x$Obesity))

listOfFunctions <- list(coefficients = coef, residuals = residuals)
f <- function(x) {sapply(listOfFunctions, function(g) g(x))}
# ziskanie jednotlivych koeficientov podmnozin datasetu
extractedData <- map(models, ~f(.x))

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

# vypocty a vizualizacie RSS a RSE-> ako dobre model sedi danym datam
rss <- map_dbl(models, ~ sum(resid(.x)^2))
boxplot(rss)
rse <- map_dbl(rss, ~ sqrt(.x/0.4*dim(hypoteza1)[1]-2))
boxplot(rse)

# test, ci existuje zavislost medzi premennymi- ci beta1 je vzdy rozna od 0
t.test(beta1_all, mu=0)
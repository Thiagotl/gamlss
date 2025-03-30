# Estudo sobre o gammls - usando o conteudo https://rpubs.com/Citro/GAMLSS

library(gamlss)
library(tidyverse)
library(ggpubr)
#library(skimr)


data('rent')
dados<-rent
View(dados)



model <- gamlss(R ~ Fl+A+H+loc, family=NO, data=rent, trace=FALSE)

plot(rent)

rent<-as.data.frame(rent)

fitted(model, "sigma")

model$sigma.link
model$sigma.coefficients

residuals(model)

plot(model)



set.seed(123)

n <- 100
x <- runif(n, 1, 10)
y <- rgamma(n, shape = 2, rate = 0.5 + 0.1 * x)  # média = shape / rate

df <- data.frame(x, y)
modelo_glm <- glm(y ~ x, family = Gamma(link = "inverse"), data = df)
summary(modelo_glm)



modelo_gamlss <- gamlss(y ~ x, family = GA, data = df)  # GA = Gama
summary(modelo_gamlss)

modelo_gamlss$mu.link











  
library(forecast)
library(fpp)
library(fpp2)
library(tidyverse)
library(ggplot2)
library(magrittr)

###############################################
### Função base para alisamento exponencial ###
###############################################

## OBS: Ajuste via minimização da soma dos erros ao quadrado (EMQ)

#### Modelos de Holt - Winters 

## HW - Aditivo (default)
(fit.ad <- HoltWinters(x= AirPassengers, seasonal = "additive"))

plot(fit.ad)		

predict(fit.ad, n.ahead = 10, prediction.interval = TRUE, 
        level = 0.95)   # previsão

## HW - Multiplicativo
(fit.mult <- HoltWinters(x= AirPassengers, seasonal = "multiplicative"))

plot(fit.mult)			

predict(fit.mult, n.ahead = 10, prediction.interval = TRUE,
        level = 0.95) #previsão


## OBS 3: Os modelos SES e Holt podem ser acessados da seguinte forma:

## SES

(fit.ses <- HoltWinters(x=AirPassengers, beta=FALSE, gamma=FALSE))

plot(fit.ses)

## Holt
(fit.holt <- HoltWinters(x=AirPassengers, gamma=FALSE))

plot(fit.holt)


## Observação: Tendência Damped não está disponível na função base


### Alternativa para plotar as previsões

x <- cbind(AirPassengers, predict(fit.ses,18),
           predict(fit.holt,18),
           predict(fit.ad,18),
           predict(fit.mult,18))
colnames(x) <- c('AirPassengers','SES','Holt','HW-aditivo', 'HW-multiplicativo')


plot(x, plot.type='single', col=c(1,2,3,4,7), lwd=2)
legend(x=1950, y = 650, legend=colnames(x), box.lwd='white', 
       col = c(1,2,3,4,7), text.col = c(1,2,3,4,7), lwd=2)


### HW-aditivo vs HW-multiplicativo

x <- cbind(AirPassengers,predict(fit.ad,18),predict(fit.mult,18) )
colnames(x) <- c('AirPassengers','HW-aditivo','HW-multiplicativo')

plot(x, plot.type='single', col=c(1,2,3), lwd=2, lty=c(1,2,3))
legend(x= 1950, y= 650, legend=colnames(x), box.lwd='white', 
       col = c(1,2,3), text.col = c(1,2,3), lwd=2, lty=c(1,2,3))


###############################################
###      Pacote forecast                   ####
###############################################

require(forecast)

## SES  --> ajuste e previsão
ses(y, h = 10, level = c(80, 95), initial = c("optimal", "simple"))


## Holt --> ajuste e previsão
holt(y, h = 10, damped = FALSE, level = c(80, 95), 
     initial = c("optimal", "simple"))

## Holt+Damped --> ajuste e previsão
holt(y, h = 10, damped = TRUE, level = c(80, 95), 
     initial = c("optimal", "simple"))

## Holt Winters --> ajuste e previsão
hw(y, h = 10, seasonal = c("additive", "multiplicative"),
   damped = FALSE, level = c(80, 95), initial = c("optimal", "simple"))

## Holt Winters + Damped --> ajuste e previsão
hw(y, h = 10, seasonal = c("additive", "multiplicative"),
   damped = TRUE, level = c(80, 95), 
   initial = c("optimal", "simple"))


#############################
### Alguns exemplos       ###
#############################

require(tseries) ## para usar a função kpss.test()


#######################
# 1) AirPassengers   ##
#######################

## HW-aditivo
fit.hw.ad <- hw(AirPassengers, h=24, seasonal = "a", initial = "o")
summary(fit.hw.ad)
plot(fit.hw.ad)

## HW-multiplicativo
fit.hw.mult <- hw(AirPassengers, h=24, seasonal = "m", initial = "o")
summary(fit.hw.mult)

plot(fit.hw.mult)

## HW-multiplicativo + Damped
fit.hw.mult.dump <- hw(AirPassengers, h=24, seasonal = "m", initial = "o", 
                       damped=T)
summary(fit.hw.mult.dump)

plot(fit.hw.mult.dump)


### seleção do modelo via AIC 
fit.hw.ad$model$aic
fit.hw.mult$model$aic
fit.hw.mult.dump$model$aic



### analise residual
E <- fit.hw.mult$residuals ## resíduos do método selecionado

# visual
par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## testes estatisticos
# Estacionaridade
kpss.test(E) # hipótese nula: série estacionária

# independência
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 3) 

# normalidade
shapiro.test(E)


#####################################
# 2) airmiles        ################
#####################################

# Série anual de utilização de milhas aéreas
plot(airmiles)

fit.ses <- ses(airmiles,h=5)
fit.ses$model
plot(fit.ses)

fit.holt <- holt(airmiles,h=5)
fit.holt$model
plot(fit.holt)

fit.damped <- holt(airmiles,h=5, damped=T)
fit.damped$model
plot(fit.damped)

### selecionando modelo utilizando AICc (Critério de Akaike Corrigido)

fit.ses$model$aicc
fit.holt$model$aicc
fit.damped$model$aicc

### analise residual
E <- fit.holt$residuals ## resíduos do método selecionado

# visual
par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## testes estatísticos

# Estacionaridade
kpss.test(E) # hipótese nula: série estacionária

# indepêndencia
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 2) 

# normalidade
shapiro.test(E)


#####################################
# 3) USAccDeaths   ##################
#####################################

# série mensal de mortes por acidentes nos EUA
plot(USAccDeaths)

## Visulamente, podemos notar:
# a) Série sazonal, com a amplitude da sazonalidade parece estabilizada (não muda muito ao longo do tempo) 
# b) Não existe tendência de crescimento/queda

## Modelo indicado (visualmente) --> Holt Winters Aditivo
## Note que a componente de crescimento poderia ser descartada (beta=FALSE)

## HW-aditivo
fit.hw.ad <- hw(USAccDeaths, h=24, seasonal = "a")
fit.hw.ad$model

## HW-multiplicativo
fit.hw.mult <- hw(USAccDeaths, h=24, seasonal = "m")
fit.hw.mult$model

### selecionando modelo utilizando AIC

fit.hw.ad$model$aic

fit.hw.mult$model$aic


### Plot modelo selecionado
plot(fit.hw.ad)


### análise residual
E <- fit.hw.ad$residuals ## resíduos do modelo selecionado

# visual
par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## testes estatisticos

# Estacionaridade
kpss.test(E) # hipótese nula: série estacionária

# independência
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 3) 

# normalidade
shapiro.test(E)


#######################################
#####  ETS: Error, Trend, Seasonal ####
#######################################

# Estrutura que permite descrever os modelos de 
# alisamento exponencial em função dos tipos de suas 
# componentes de erro, tendência e sazonalidade

require(forecast)
require(tseries)

# ets(y, model = "ZZZ", damped = NULL, alpha = NULL, 
#           beta = NULL, gamma = NULL, phi = NULL, 
#           allow.multiplicative.trend = FALSE)


# Exemplos via ETS 
###  SES: ets(y, model = "ANN", damped=FALSE)
###  Holt: ets(y, model = "AAN", damped=FALSE)
###  Holt + Damped: ets(y, model = "AAN", damped=TRUE)
###  HW Aditivo: ets(y, model = "AAA", damped=FALSE)
###  HW Multip.: ets(y, model = "MAM", damped=FALSE)

## São proíbidas na função ets(): ETS(A;N;M), ETS(A;A;M),
#ETS(A;Ad;M); pois algumas combinações de erro, tendência e
#sazonalidade podem apresentar problemas numéricos, em 
#especial aquelas que misturam erro aditivo com sazonalidade
#multiplicativa.


## Ajuste

fit <- ets(AirPassengers)

summary(fit)

## plot das componentes
autoplot(fit)

## Ajuste do modelo graficamente

plot(AirPassengers)
lines(fitted(fit), col= "red")

### análise de resíduos para o método selecionado
E <- fit$residuals 

par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## Testes estatísticos

# Estacionaridade
kpss.test(E) # hipótese nula: série estacionária

# independencia
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 3)

# normalidade
shapiro.test(E)

### Previsão 
prev <- forecast(fit, h=24, level = c(80, 95))

summary(prev)

plot(prev)

### previsões intervalares não paramétricas (bootstrap via reamostragem dos erros)

prev <- forecast(fit, h=24, level = c(80, 95), bootstrap = TRUE)

summary(prev)

plot(prev)

############# "Comparando" com um SARIMA

fit_1 <- Arima(AirPassengers, order= c(1,1,1), 
               seasonal = c(0,1,0), 
               include.mean = FALSE)

summary(fit_1)


#### Exercício: estudar as séries

require(fpp2)

plot(bicoal)

plot(chicken)

plot(dole)

plot(usdeaths)

plot(bricksq)

plot(lynx)

plot(eggs)

plot(ausbeer)

plot(debitcards)




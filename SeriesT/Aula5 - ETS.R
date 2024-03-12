
#######################################
#####  ETS: Error, Trend, Seasonal ####
#######################################

# Estrutura que permite descrever os modelos de 
# alisamento exponencial em fun??o dos tipos de suas 
# componentes de erro, tend?ncia e sazonalidade

require(forecast)
require(tseries)

# ets(y, model = "ZZZ", damped = NULL, alpha = NULL,  # ZZZ para testar td aleatorio
#           beta = NULL, gamma = NULL, phi = NULL,
#           allow.multiplicative.trend = FALSE)


# Exemplos via ETS - A: Aditivo; N: Neutral; M: Multiplicativo
###  SES: ets(y, model = "ANN", damped=FALSE)
###  Holt: ets(y, model = "AAN", damped=FALSE)
###  Holt + Damped: ets(y, model = "AAN", damped=TRUE)
###  HW Aditivo: ets(y, model = "AAA", damped=FALSE)
###  HW Multip.: ets(y, model = "MAM", damped=FALSE)

## S?o pro?bidas na fun??o ets(): ETS(A;N;M), ETS(A;A;M),
#ETS(A;Ad;M); pois algumas combina??es de erro, tend?ncia e
#sazonalidade podem apresentar problemas num?ricos, em 
#especial aquelas que misturam erro aditivo com sazonalidade
#multiplicativa.


## Ajuste

fit <- ets(AirPassengers)

summary(fit) # Smoothing: suavizacao

## plot das componentes
autoplot(fit)

## Ajuste do modelo graficamente

plot(AirPassengers)
lines(fitted(fit), col= "red")

### an?lise de res?duos para o m?todo selecionado
E <- fit$residuals 

par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## Testes estat?sticos

# Estacionaridade
kpss.test(E) # hip?tese nula: s?rie estacion?ria

# independencia
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 3)

# normalidade
shapiro.test(E)

### Previs?o 
prev <- forecast(fit, h=24, level = c(80, 95))

summary(prev)

plot(prev)

### previs?es intervalares n?o param?tricas (bootstrap via reamostragem dos erros)

prev <- forecast(fit, h=24, level = c(80, 95), bootstrap = TRUE)

summary(prev)

plot(prev)

############# "Comparando" com um SARIMA

fit_1 <- Arima(AirPassengers, order= c(1,1,1), 
               seasonal = c(0,1,0), 
               include.mean = FALSE)

summary(fit_1)


#### Exerc?cio: estudar as s?ries

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




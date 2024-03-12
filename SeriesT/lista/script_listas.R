# setwd('./SeriesT/lista')

# Pacotes -----------------------------------------------------------------
packages <- c('readxl', "tseries", "timeSeries", "ggplot2", "forecast", "fpp2", 
              "tidyverse", "lmtest", 'magrittr', 'fpp')
lapply(packages, library, character.only = TRUE)


# Lista 1 - Ex1 -----------------------------------------------------------
dados1 <- read_xlsx('1.xlsx')
consumo <- as.ts(dados1[,3])
x1 <- ts(consumo, frequency=4, start=c(1970,1))

## a
autoplot(x1) + xlab("Anos") + ylab("Consumo")
# Podemos observar que essa serie tem um comportamento estacionario, nao identifico uma tendencia.

ggAcf(x1) # MA: Podemos ver que ha uma auto correlacao presente nos lags, teremos como chute um valor de 3.
ggPacf(x1) # AR: Para o termo AR, o chute escolhido sera de 2.

## b
# Nao vejo necessidade de aplicar transformacoes nem diferenciacoes no momento, podemos checar com:
d <- ndiffs(x1, alpha = 0.05); d # Sem diferenciacoes
kpss.test(x1) # Serie estacionaria

## c
# Como comentado no item a, um modelo a ser testado inicialmente seria o ARIMA(2,0,3), dado que temos dois lags
#na parcial e 3 na normal, indicando possiveis valores de parametros a serem testados.

## d
mod_1 <- Arima(x1, order = c(2, 0, 3))

## e
fit.arma <- Arima(x1, order = c(2, 0, 3), include.mean = FALSE)

E <- fit.arma$residuals

checkresiduals(fit.arma)

## Testes estat?sticos

# Estacionaridade
kpss.test(E)

# independ?ncia
Box.test(E, lag = 20, type ="Ljung-Box", fitdf = 5) # AR = 2 + MA = 3

# normalidade
shapiro.test(E) # Nao normal, mas estamos preocupados mais com a variancia
# Como no grafico a normalidade esta dentro da normalidade, vamos manter o modelo

# g
mod_1 %>% forecast(h = 24, level = 95) %>% plot()


# Lista 1 - Ex2 -----------------------------------------------------------
dados2 <- read_xlsx('2.xlsx')
x2 <- as.ts(dados2)
autoplot(x2)

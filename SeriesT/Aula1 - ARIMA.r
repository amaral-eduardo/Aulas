
rm(list=ls(all=TRUE))

packages <- c("readxl", "tseries", "timeSeries", "ggplot2", "forecast", "fpp2", 
              "tidyverse")

if(length(setdiff(packages, rownames(installed.packages()))) > 0){
  install.packages(setdiff(packages, rownames(installed.packages())))
} #Verifica se est? instalado, se n?o estiver, vai instalar

lapply(packages, library, character.only = TRUE)


###########################################################################

data() # Listando os conjuntos de dados do R

data(package = 'tseries') # Listando os conjuntos de dados de um pacote de ST do R

# Importando dados de uma ST para o R
y <- scan(file='./SeriesT/Fig1.2.Recife.txt')

#Criando uma ST a partir dos dados importados
x <- as.ts(y)

#Verificando se o arquivo tem a estrutura de uma ST
class(y)
is.ts(y)
is.ts(x)

# Criando uma ST mensal iniciando em janeiro de 1953

x1 <- ts(x, frequency=12, start=c(1953,1)) # Frequencia igual 12 indica que ela foi coletada mes a mes
## Caso colocasse '1', seria indicando que foi uma coleta por ano.

plot(x1,
     type='l',
     lwd=2,
     col='blue',
     xlab='Anos',
     ylim=c(22,30),
     ylab='Temperatura (oC)',
     main='Temperatura m?dia do ar em Recife, em meses sucessivos')

#ou

autoplot(x1) +
  xlab("Anos") + ylab("Temperatura (oC)")
ggAcf(x1) # Serve para definir o chute inicial para medias moveis, mas depois de deixar estacionaria
ggPacf(x1) # Chute para o modelo auto regressivo

#ou 
library(dygraphs)

dygraph(x1, main = " ", xlab='Anos', ylab='Temperatura (oC)') %>% 
  dyAxis("x", drawGrid = FALSE) %>% 
  dyEvent("1960-4-01", "1960", labelLoc = "bottom") %>% # Destacar no grafico um ano especifico
  dyOptions(drawPoints = TRUE, pointSize = 2)



#Exemplos de dados anuais, trimestrais e mensais:

x2 <- ts(x1, frequency= 1, start=1960) # Frequencia 1 implica em anual, como dito anteriormente
plot(x2)

x3 <- ts(x1, frequency= 4, start=c(1960,2))   # Inicia no 2o. trimestre de 60 # Dados trimestrais
plot(x3)

x4 <- ts(x1, frequency= 12, start=c(1960,10)) # Inicia em outubro de 1960 # Mes a mes a coleta
plot(x4)

# Semanas fixa em 52

# Primeira Diferen?a => Linear
plot(diff(y),type='l',lwd=2,col='blue',xlab='',ylim=c(0,30),ylab='diff(y)', main='')

# Segunda Diferen?a => Linear
plot(diff(diff(y)),type='l',lwd=2,col='blue',xlab='',ylim=c(0,10),ylab='diff(diff(y))', main='')


#No banco de dados do R, a serie lh cont?m as quantidades de um tipo de horm?nio
#em amostras de sangue coletadas a cada 10 minutos de uma pessoa do sexo feminino
#(Diggle, 1990). Identifique um modelo ARIMA para esta s?rie temporal.

data(lh)

is.ts(lh)

par(mfrow = c(3,1))
plot(lh,xlab='tempo',ylab='observa??es',main='') # Se pa que nao estacionaria, ver nos testes a seguir
acf (lh,xlab='defasagem',ylab='FAC',main='') # Chute de ordem 1 para modelo medias moveis
pacf(lh,xlab='defasagem',ylab='FACP',main='') # Chute de ordem 1 para modelo autoregressivo
par(mfrow = c(1,1))

astsa::acf2(lh)

#A s?rie precisa ser diferenciada?

#require(forecast)


# Testes pra estacionaridade ----------------------------------------------

(d <- ndiffs(lh, alpha = 0.05)) #n?mero de diferen?as necess?rias
# Nao precisa diferenciar a serie, ja e estacionaria

kpss.test(lh) # hip?tese nula: s?rie estacion?ria;
# p-valor>0.05 -> serie estacionaria

adf.test(lh) # serie estacionaria

pp.test(lh) # serie estacionaria


# Codigo para deixar a serie estacionaria ---------------------------------

## caso precise diferenciar a s?rie 
#w <- diff(lh, differences = d )

#par(mfrow = c(3,1))
#plot(w,xlab='tempo',ylab='Diff observa??es',main='')
#acf (w,xlab='defasagem',ylab='FAC',main='')
#pacf(w,xlab='defasagem',ylab='FACP',main='')
#par(mfrow = c(1,1))

################################################
# Ajustando o modelo
################################################

(mod_1 <- arima(lh, order = c(1,0,0))) # Modelo AR
# 1 -> Parte AR
# 0 -> Quantidade de diferencia??es realizadas
# 0 -> Parte de MA - Medias moveis

#tsdiag(mod_1) #script cont?m erros, n?o ? recomend?vel us?-lo

(mod_2 <- arima(lh, order = c(0,0,1))) # Modelo MA

(mod_3 <- arima(lh, order = c(1,0,1))) # Modelo ARMA

# ou  forecast::Arima(lh, order = c(1,0,0))

# Verificando com o auto.arima

#require("forecast")
(Ajuste_AutoArima = auto.arima(lh))

################################################
##### An?lise de Res?duos
################################################

fit.arma <- arima(lh, order=c(1,0,0), include.mean = FALSE)

E <- fit.arma$residuals

checkresiduals(fit.arma)

## Testes estat?sticos

# Estacionaridade
kpss.test(E) # hip?tese nula: s?rie estacion?ria

adf.test(E)

pp.test(E)

# independ?ncia
Box.test(E, lag = 20, type ="Ljung-Box", fitdf = 1) ## use fitdf= p+q, no caso 1+0
# Fitdf 'e o numero de parametros que foram utilizados AR = 1 = p; IM = 0 = q

# normalidade
shapiro.test(E) # Nao normal, mas estamos preocupados mais com a variancia
# Como no grafico a normalidade esta dentro da normalidade, vamos manter o modelo



# Aula 01/02 --------------------------------------------------------------

### Predição - FOrecast
prev_lh <- forecast(mod_1, h = 15, level = c(80, 95))
prev_lh
plot(prev_lh, ylab = 'lh')

### Bandas de confian'ca muitos grandes









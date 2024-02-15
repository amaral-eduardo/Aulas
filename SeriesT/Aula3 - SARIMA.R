rm(list=ls(all=TRUE))

packages <- c("readxl", "tseries", "timeSeries", "ggplot2", 
              "forecast", "fpp2", 
              "tidyverse", "lmtest")

if(length(setdiff(packages, rownames(installed.packages()))) > 0){
  install.packages(setdiff(packages, rownames(installed.packages())))
} #Verifica se est? instalado, se n?o estiver, vai instalar

lapply(packages, library, character.only = TRUE)

########################################################
#######    Modelos SARIMA(p,d,q)x(P,D,Q)s     ##########
########################################################

# Monthly Airline Passenger Numbers 1949-1960 (Box, Jenkins and Reinsel, 1976).
# Totais mensais de passageiros de linhas a?reas internacionais nos EUA, 1949-1960

x <- AirPassengers

is.ts(x) # Verifica sempre primeiro se e uma serie temporal 

#plot(x)
autoplot(x) +
  xlab("Anos") + ylab("N?mero de passageiros")

# Variancia vai aumentando com o tempo.
# Serie crescente
# Sazonal

#ggAcf(x)
ggtsdisplay(x)
# Os picos da ACF sempre sao nos multiplos de 12, indica que nossa serie e sazonal
#com periodo 12. 

# Existe uma clara tend?ncia de crescimento bem como um 
#padr?o sazonal ao longo dos anos. 


#Verificando a sazonalidade

x %>% decompose() %>% autoplot() # Decompoe a serie

#plot(lynx) # Visualmente ocorre uma sazonalidade, mas como veremos:

#lynx %>% decompose() %>% autoplot() # A serie nao apresenta sazonalidade

#lynx %>% stl(s.window = "periodic") %>% autoplot() # Talvez o ciclo nao e fixo


####################
#### Diferen?as ####
####################

# primeira diferen?a linear

#ndiffs(x)

dx <- diff(x)

par(mfrow=c(3,1))
plot(dx, main='dx')
acf(dx, lag=5*12)
pacf(dx, lag=5*12)
par(mfrow=c(1,1))

#require(tseries) ## para usar a fun??o kpss.test()
kpss.test(dx)    # hip?tese nula: s?rie estacion?ria

###### Confiem nos "seus olhos"!

# diferen?a sazonal
#require(forecast)

nsdiffs(dx) ## ou nsdiffs(x) --> tend?ncia removida por decomposi??o
# Quantas diferenciacoes ele recomenda

d12dx <- diff(dx,lag=12)

par(mfrow=c(3,1))         # Para p e q, vemos de forma padrao, mas para P e Q
plot(d12dx, main='d12dx') #vemos de forma sazonal, ou seja, vendo a ACF temos que:
acf(d12dx, lag=5*12)      #para o q o chute seria 0 ou 1, Q seria 0. Pela PACF:
pacf(d12dx, lag=5*12)     #p = 0 ou 1, P = 0.
par(mfrow=c(1,1))

# fazendo os dois: d12dx <- diff(diff(x, lag= 12))

################################
### Modelos Candidatos #########
################################

# Sabemos que d = 1 e D = 1, al?m disso:

# p = 1      --> ACF decaindo rapidamente para zero e PACF com pequena "quebra" no lag 1
# q = 0 ou 1 --> PACF com "quebra" no lag 1
# P = 0      --> ACF e PACF sem autocorela??es nos lags sazonais
# Q = 0      --> ACF e PACF sem autocorela??es nos lags sazonais  


## Modelos candidatos para a s?rie
# Modelo 1: SARIMA (1,1,0) x (0,1,0)
# Modelo 2: SARIMA (1,1,1) x (0,1,0)


##############################################
## Modelo 1: SARIMA (1,1,0) x (0,1,0)
##############################################
mod_1 <- Arima(x, order= c(1,1,0), seasonal = c(0,1,0), 
              include.mean = FALSE)
mod_1

#require(lmtest)
coeftest(mod_1)

# res?duos

# checkresiduals(mod_1)

E <- mod_1$residuals
plot(E) # Esses zeros sao para dar um start no modelo, podemos retirar ele da forma:

E <- window(E,  start=time(x)[14]) #come?a em 14, pois perdemos 12+1 res?duos
plot(E)


## an?lise visual
par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E) # Tao tudo OK
qqline(E)
par(mfrow=c(1,1))



## testes estat?sticos
# Estacionaridade
kpss.test(E) # hip?tese nula: s?rie estacion?ria
# independ?ncia
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 1) 
# normalidade
shapiro.test(E)


##############################################
## Modelo 2: SARIMA (1,1,1) x (0,1,0)
##############################################
mod_2 <- Arima(x, order=c(1,1,1), seasonal = c(0,1,0), include.mean = FALSE)
mod_2

# res?duos
E <- mod_2$residuals
plot(E)

E <- window(E,  start=time(x)[14])
plot(E)


## an?lise visual
par(mfrow=c(2,2))
plot(E)
acf(E)
pacf(E)
qqnorm(E)
qqline(E)
par(mfrow=c(1,1))

## testes estat?sticos

# Estacionaridade
kpss.test(E) # hip?tese nula: s?rie estacion?ria

# independ?ncia
Box.test(E, lag = 15, type ="Ljung-Box", fitdf = 2) 

# normalidade
shapiro.test(E)



###########################################
### Crit?rio de sele??o de modelos: AIC ###
###########################################

mod_1 # --> escolhido pelo AIC
mod_2

#auto.arima(x, trace= TRUE)

autoplot(x) +
  autolayer(mod_1$fitted, series= "SARIMA(1,1,0)(0,1,0)12")

#ou

v <- mod_1$residuals
ajuste <- x - v

plot(x, main = "", xlab="Anos", ylab="N?mero de passageiros", 
     col= "red")
lines(ajuste, col="blue",type= "l")
legend(1957, 200, c("S?rie original", "SARIMA(1,1,0)(0,1,0)12"),
       col= c('red', 'blue'), pch=rep(20,2))


################
### Previs?o ###
################

#require(forecast) 

prev <- arima(x, order= c(1,1,0),
              seasonal = c(0,1,0))

prev %>% predict(n.ahead= 12) # Fazendo previsao para 12 meses a frente

# ou

mod_1 %>% forecast(h= 12, level= 95) %>% plot() # 12 Meses
mod_1 %>% forecast(h= 24, level= 95) %>% plot() # 24 Meses
mod_1 %>% forecast(h= 36, level= 95) %>% plot() # 36 Meses

# Conforme aumenta a quantidade de meses, nosso erro vai aumentando, recomenda-se
#usar no maximo 24 meses no caso de uma serie com sazonalidade anual.

#Sempre tomando bastante cuidado com estimacoes mais longas.

####### Exerc?cio: 

# Verifique o melhor modelo para log(AirPassengers) 


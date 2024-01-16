#install.packages('autoReg')
require(autoReg)

### Tempo (semanas) em remissão de leucemia
dados <- anderson
dados


tempo <- dados$time


##################################################################
### Estimativas de S(t) para o tempo1  ###########################
##################################################################

tempo1 <- subset(dados, rx==1, select=c('time', 'status'))
tempo1

S0 <- 1
S1 <- mean(tempo1$time>1)
S2 <- mean(tempo1$time>2)
S3 <- mean(tempo1$time>3)
S4 <- mean(tempo1$time>4)
S5 <- mean(tempo1$time>5)
S8 <- mean(tempo1$time>8)
#...
S23 <- mean(tempo1$time>23)

c(S0, S1, S2, S3, S4, S5, S8, S23)


### Kaplan-Meier
S0 <- 1
S1 <- 1 - 2/21    ; S1
S2 <- (1-2/19)*S1 ; S2
S3 <- (1-1/17)*S2 ; S3



### Plotando S(t) estimada
require(survival)
dados_sob <- Surv(tempo1$time, tempo1$status)


km1 <- survfit(dados_sob~1, conf.type='plain')
summary(km1)

plot(km1,  mark.time=T, conf.int=T)


##################################################################
### Estimativas de S(t) para o tempo0  ###########################
##################################################################

tempo0 <- subset(dados, rx==0, select=c('time', 'status'))


S0 <- 1
S6 <- 1 - 3/21    ; S6
S7 <- (1-1/17)*S6 ; S7
S10 <- (1-1/15)*S7 ; S10




### Plotando S(t) estimada
dados_sob <- Surv(tempo0$time, tempo0$status)


km1 <- survfit(dados_sob~1, conf.type='plain')
km2 <- survfit(dados_sob~1, conf.type='log-log')

summary(km1)
summary(km2)

plot(km1,  mark.time=T, conf.int=T)
lines(km2, mark.time=T, conf.int=T, col=2)


surv <- km1$surv
time <- km1$time

vida_media <- 1 + sum(surv[-12]*diff(time))
vida_media


### Comparando grupos
require(autoReg)
require(survival)

### Tempo (semanas) em remissão de leucemia
dados <- anderson
head(dados)

Y <- Surv(dados$time, dados$status)
X <- ifelse(dados$rx==0, 'Tratamento', 'Placebo')

KM <- survfit(Y~X)
KM

plot(KM, mark.time=T, conf.int=F, lwd=2, col=1:2, xlab='Tempo em remissão', ylab='Prob. de sobrevida estimada')
legend('topright', c('1-Placebo', '0-Tratamento'), col=1:2, lwd=2, bty='n')


### Teste Log rank
survdiff(Y~X)

### Outros testes
# install.packages('survMisc')
library(survMisc)
comp(ten(KM))


### Estimando função de risco
require(muhaz)

h0 <- muhaz(dados$time, dados$status, subset=c(dados$rx==0), bw.method='local', b.cor='both')
h1 <- muhaz(dados$time, dados$status, subset=c(dados$rx==1), bw.method='local', b.cor='both')

plot(h0, ylim=c(0,0.12))
lines(h1, col=2)

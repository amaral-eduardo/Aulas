rm(list=ls())
require(survival)
require(muhaz)
require(survMisc)

### Tempo (dias) de sobrevida dos camundongos
dados <- read.table('camun.txt', h=T)
head(dados)


### Sem covariáveis
Y <- Surv(dados$Tempo, dados$Censura)

km <- survfit(Y~1)
na <- survfit(Y~1, type='fh')

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
legend(15,0.8, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')


h0 <- muhaz(dados$Tempo, dados$Censura)
plot(h0, lwd=2, ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')


### Análise Paramétrica
### Distribuição Exponencial

theta_hat <- 1/(sum(dados$Tempo)/sum(dados$Censura))

tt <- seq(0, max(dados$Tempo), 0.1)
ss <- 1 - pexp(tt, rate=theta_hat)
hh <- dexp(tt, rate=theta_hat)/ss

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt,ss, lwd=2, col=2)
legend(15,0.8, c('Kaplan-Meier', 'Exponencial'), lwd=2, col=1:2, bty='n', cex=1.2)

plot(h0, lwd=2, ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')
lines(tt, hh, lwd=2, col=2)
legend('left', c('Não-parmétrica', 'Exponencial'), lwd=2, col=1:2, bty='n', cex=1.2)


### Tempo médio de sobrevida (estimado)
### E(T) = 1/theta

tm <- 1/theta_hat
tm


### Probabilidade (estimada) de Sobreviver mais que 10 dias
### S(10) = exp(-theta*10)

S10 <- exp(-theta_hat*10)
S10


### Weibull
### install.packages('parmsurvfit')
require(parmsurvfit)

fit_weibull <- fit_data(data=dados, dist="weibull", time="Tempo", censor="Censura")

shape_hat <- fit_weibull$estimate[1]
scale_hat <- fit_weibull$estimate[2]

tt <- seq(0, max(dados$Tempo), 0.1)
ss1 <- 1 - pweibull(tt, shape=shape_hat, scale=scale_hat)
hh <- dweibull(tt, shape=shape_hat, scale=scale_hat)/ss1

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt, ss1, lwd=2, col=2)
legend(15,0.8, c('Kaplan-Meier', 'Weibull'), lwd=2, col=1:2, bty='n', cex=1.2)

plot(h0, lwd=2, xlim=c(0,25), ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')
lines(tt, hh, lwd=2, col=2)
legend('left', c('Não-parmétrica', 'Weibull'), lwd=2, col=1:2, bty='n', cex=1.2)


### Log-normal

hlnorm <- function(t, mu, si) dlnorm(t, mu, si)/(1-plnorm(t, mu, si))

mu <- 0:2
si <- 2

tt <- seq(0, 4, 0.01)

plot(hlnorm(tt, mu[1], si)~tt, type='l', lwd=2, ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')
lines(hlnorm(tt, mu[2], si)~tt, lwd=2, col=2)
lines(hlnorm(tt, mu[3], si)~tt, lwd=2, col=3)
legend('topright', c('0', '1', '2'), title=expression(mu), lwd=2, col=1:3, bty='n', cex=1.2)



fit_lnorm <- fit_data(data=dados, dist="lnorm", time="Tempo", censor="Censura")

mu_hat <- fit_lnorm$estimate[1]
si_hat <- fit_lnorm$estimate[2]

tt <- seq(0, max(dados$Tempo), 0.1)
ss2 <- 1 - plnorm(tt, mu_hat, si_hat)
hh <- 	  dlnorm(tt, mu_hat, si_hat)/ss2

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt, ss2, lwd=2, col=2)
legend(15,0.8, c('Kaplan-Meier', 'LogNormal'), lwd=2, col=1:2, bty='n', cex=1.2)

plot(h0, lwd=2, xlim=c(0,25), ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')
lines(tt, hh, lwd=2, col=2)
legend('left', c('Não-parmétrica', 'LogNormal'), lwd=2, col=1:2, bty='n', cex=1.2)

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt, ss0, lwd=2, col=2)
lines(tt, ss1, lwd=2, col=3)
lines(tt, ss2, lwd=2, col=4)
legend(15,0.8, c('Kaplan-Meier', 'Expon', 'Weibull', 'LogNormal'), lwd=2, col=1:4, bty='n', cex=1.2)




### Log-logistic

dll <- function(t, alpha, beta) {alpha*t^(alpha-1)/beta^alpha / (1+ (t/beta)^alpha)^2}
sll <- function(t, alpha, beta) {1 / (1+ (t/beta)^alpha)}
hll <- function(t, alpha, beta) dll(t, alpha, beta)/sll(t, alpha, beta)


### density
al <- c(1,5,10)
be <- 1

tt <- seq(0,5,0.01)
ff1 <- dll(tt, al[1], be)
ff2 <- dll(tt, al[2], be)
ff3 <- dll(tt, al[3], be)

plot(tt, ff1, t='l', lwd=2, ylim=c(0,2), xlab='Tempo', ylab='Densidade')
lines(tt, ff2, lwd=2, col=2)
lines(tt, ff3, lwd=2, col=3)


al <- 2
be <- c(1/2, 1, 2)

tt <- seq(0,5,0.01)
ff1 <- dll(tt, al, be[1])
ff2 <- dll(tt, al, be[2])
ff3 <- dll(tt, al, be[3])

plot(tt, ff1, t='l', lwd=2, ylim=c(0,1.5), xlab='Tempo', ylab='Densidade')
lines(tt, ff2, lwd=2, col=2)
lines(tt, ff3, lwd=2, col=3)


### hazard
al <- c(1,2,8)
be <- 1

tt <- seq(0,5,0.01)
hh1 <- hll(tt, al[1], be)
hh2 <- hll(tt, al[2], be)
hh3 <- hll(tt, al[3], be)

plot(tt,  hh1, t='l', lwd=2, ylim=c(0,4), xlab='Tempo', ylab='Risco - h(t)')
lines(tt, hh2, lwd=2, col=2)
lines(tt, hh3, lwd=2, col=3)


al <- 2
be <- c(1/2, 1, 2)

tt <- seq(0,5,0.01)
hh1 <- hll(tt, al, be[1])
hh2 <- hll(tt, al, be[2])
hh3 <- hll(tt, al, be[3])

plot(tt,  hh1, t='l', lwd=2, ylim=c(0,1.5), xlab='Tempo', ylab='Risco - h(t)')
lines(tt, hh2, lwd=2, col=2)
lines(tt, hh3, lwd=2, col=3)




### Generalized Gamma
require(flexsurv)

### density

tt <- seq(0,5,0.01)
ff1 <- dgengamma.orig(tt, shape=1 , scale=2 , k=1)
ff2 <- dgengamma.orig(tt, shape=2 , scale=2 , k=1)
ff3 <- dgengamma.orig(tt, shape=1 , scale=2 , k=2)
ff4 <- dgengamma.orig(tt, shape=2 , scale=2 , k=2)

plot(tt, ff1, t='l', lwd=2, ylim=c(0,0.5), xlab='Tempo', ylab='Densidade')
lines(tt, ff2, lwd=2, col=2)
lines(tt, ff3, lwd=2, col=3)
lines(tt, ff4, lwd=2, col=4)
legend('topright', c('Exponencial(1/2)', 'Weibull(2,2)', 'Gama(2,2)', 'GG(2,2,2)'), lwd=2, col=1:4, bty='n', cex=1.2)

fit_gg <- flexsurvreg(Y~1, dist='gengamma')
fit_gg


fit_ll <- flexsurvreg(Y~1, dist='llogis')
fit_ll


tt <- seq(0, max(dados$Tempo), 0.1)

plot(fit_gg, col=5, ci=F, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(fit_ll, lwd=2, col=6, ci=F)
lines(tt, ss0, lwd=2, col=2)
lines(tt, ss1, lwd=2, col=3)
lines(tt, ss2, lwd=2, col=4)
legend(15,0.8, c('Kaplan-Meier', 'Expon', 'Weibull', 'LogNormal', 'GenGamma', 'LogLogist'), lwd=2, col=1:6, bty='n', cex=1.2)

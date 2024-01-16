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

### EMV da exponencial para dados censurados
theta_hat <- 1/(sum(dados$Tempo)/sum(dados$Censura))
theta_hat

sd_theta_hat <- theta_hat/sqrt(sum(dados$Censura))
sd_theta_hat

ic_theta <- c(theta_hat - qnorm(0.975)*sd_theta_hat, theta_hat + qnorm(0.975)*sd_theta_hat)
ic_theta


loglik_expon <- function(theta, data) {

t <- data$Tempo
d <- data$Censura

r <- sum(d)

ll <- r*log(theta) - theta*sum(t)

return(-ll)
}

# loglik_expon(theta_hat, data=dados)

### EMV da exponencial usando metodos numericos
emv <- optim(1, loglik_expon, method='Brent', lower=0, upper=10000, data=dados)
emv

sd_emv <- sqrt(1/optimHess(theta_hat, loglik_expon, data=dados))
sd_emv

emv <- optim(1, loglik_expon, method='Brent', lower=0, upper=10000, data=dados, hessian=T)
emv

tt <- seq(0, max(dados$Tempo), 0.1)
ss0 <- 1 - pexp(tt, rate=theta_hat)
hh <- dexp(tt, rate=theta_hat)/ss0

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt,ss0, lwd=2, col=2)
legend(15,0.8, c('Kaplan-Meier', 'Exponencial'), lwd=2, col=1:2, bty='n', cex=1.2)


### Tempo médio de sobrevida (estimado)
### E(T) = 1/theta

tm <- 1/theta_hat
tm

### Desvio padrao do tempo medio (metodo delta)
var_tm <- 1/(sum(dados$Censura)*theta_hat^2)
sd_tm  <- sqrt(var_tm)
sd_tm

### IC para o tempo medio
ic_tm <- c(tm - qnorm(0.975)*sd_tm, tm + qnorm(0.975)*sd_tm)
ic_tm


### Probabilidade (estimada) de Sobreviver mais que 10 dias
### S(10) = exp(-theta*10)
S10 <- exp(-theta_hat*10)
S10



### Weibull
### install.packages('parmsurvfit')
loglik_weibull <- function(theta, data) {

alpha <- theta[1]
beta  <- theta[2]

t <- data$Tempo
d <- data$Censura

r <- sum(d)

ll <- r*(log(alpha) - alpha*log(beta)) + (alpha-1)*sum(d*log(t)) - (beta^(-alpha))*sum(t^alpha)

return(-ll)
}

ini <- c(1,1)

emv <- optim(ini, loglik_weibull, method='L-BFGS-B', lower=0.01, upper=Inf, data=dados, hessian=T)
emv

alpha_hat <- emv$par[1]
beta_hat  <- emv$par[2]
loglik    <- -emv$value
c(Alpha=alpha_hat, Beta=beta_hat, LogVeross=loglik)

hes <- emv$hessian ; hes
var <- solve(hes) ; var

sd_alpha <- sqrt(var[1,1]); sd_alpha
sd_beta  <- sqrt(var[2,2]); sd_beta


require(parmsurvfit)

fit_weibull <- fit_data(data=dados, dist="weibull", time="Tempo", censor="Censura")
summary(fit_weibull)


shape_hat <- fit_weibull$estimate[1]
scale_hat <- fit_weibull$estimate[2]





tt <- seq(0, max(dados$Tempo), 0.1)
ss1 <- 1 - pweibull(tt, shape=shape_hat, scale=scale_hat)
hh <- dweibull(tt, shape=shape_hat, scale=scale_hat)/ss

plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(tt, ss, lwd=2, col=2)
legend(15,0.8, c('Kaplan-Meier', 'Weibull'), lwd=2, col=1:2, bty='n', cex=1.2)

plot(h0, lwd=2, xlim=c(0,25), ylab='Função de Risco Estimada', xlab='Tempo de sobrevida')
lines(tt, hh, lwd=2, col=2)
legend('left', c('Não-parmétrica', 'Weibull'), lwd=2, col=1:2, bty='n', cex=1.2)




### Comparando Exponencial e Weibull
### TRV

lweib <- -loglik_weibull(c(alpha_hat, beta_hat), data=dados)
lexpo <- -loglik_expon(theta_hat, data=dados)
 
lambda <- 2*(lweib-lexpo)
lambda

qchisq(0.95, df=1)


pvalor <- 1-pchisq(lambda, df=1)
pvalor











# Fonte dos dados ---------------------------------------------------------
# http://sobrevida.fiocruz.br/uti.html
dados <- read.table("http://sobrevida.fiocruz.br/dados/ctinca.dat", header = TRUE)
head(dados)

# Pacotes
require(survival)
require(muhaz)
require(pander)
require(flexsurv)
require(hnp)
require(ggplot2)
require(survminer)
require(parmsurvfit)

# Traducao dos dados ------------------------------------------------------
dados$sexo <- ifelse(dados$sexo == "Male","Mas","Fem")
dados$desnut <- ifelse(dados$desnut == "n", "Não", "Sim")
dados$comorbi <- ifelse(dados$comorbi == "n", "Não", "Sim")
dados$leucopenia <- ifelse(dados$leucopenia == "n", "Não", "Sim")

dados$sexo <- factor(dados$sexo, levels = c("Mas","Fem"))
dados$desnut <- factor(dados$desnut, levels = c("Sim","Não"))
dados$comorbi <- factor(dados$comorbi, levels = c("Sim","Não"))
dados$leucopenia <- factor(dados$leucopenia, levels = c("Sim","Não"))
dados$gptumor <- factor(dados$gptumor, levels = c("Loco", "Mtx", "Hemato"))

summary(dados)

str(dados)

# Escolha da variavel -----------------------------------------------------
# Variavel explicativa para separar os dados em 3 grupos

Loco <- subset(dados, gptumor=="Loco", select=c('tempo', 'status'))
Mtx <- subset(dados, gptumor=="Mtx", select=c('tempo', 'status'))
Hemato <- subset(dados, gptumor=="Hemato", select=c('tempo', 'status'))

dados1 <- dados[,c('tempo', 'status', 'gptumor')]

dados_sob <- Surv(dados1$tempo, dados1$status)
Loco_sob <- Surv(Loco$tempo, Loco$status)
Mtx_sob <- Surv(Mtx$tempo, Mtx$status)
Hemato_sob <- Surv(Hemato$tempo, Hemato$status)

# Analise descritiva ------------------------------------------------------

# Funcao de sobrevivencia -------------------------------------------------
km1 <- survfit(dados_sob~1)
km2 <- survfit(Loco_sob~1)
km3 <- survfit(Mtx_sob~1)
km4 <- survfit(Hemato_sob~1)

plot(km2, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida',
     ylab='Prob. de sobrevida estimada', main = 'Função de Sobrevivência', col = 4)
lines(km3, mark.time=T, conf.int=F, lwd=2, col=3)
lines(km4, mark.time=T, conf.int=F, lwd=2, col=2)
lines(km1, mark.time=T, conf.int=F, lwd=2, col=1)
legend(130,0.95, paste(c('Sólido localizado', 'Metastático', 'Hematológico',
                         'Completo')), lwd=2, col=4:1, bty='o')

# Funcao de risco ---------------------------------------------------------
# Completo
h0 <- muhaz(dados1$tempo, dados1$status, min.time = 1, max.time = 182)

# Loco
h1 <- muhaz(Loco$tempo, Loco$status, min.time = 1, max.time = 182)

# Hemato
h2 <- muhaz(Hemato$tempo, Hemato$status, min.time = 1, max.time = 182)

# Mtx
h3 <- muhaz(Mtx$tempo, Mtx$status, min.time = 1, max.time = 182)

# Graficos
plot(h1, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida',
     ylab='Função de Risco Estimada', col=4, main = "Funçao de Risco")
lines(h3, mark.time=T, conf.int=F, lwd=2, col=3)
lines(h2, mark.time=T, conf.int=F, lwd=2, col=2)
lines(h0, mark.time=T, conf.int=F, lwd=2, col=1)
legend(130,0.025, paste(c('Sólido localizado', 'Metastático', 'Hematológico',
                         'Completo')), lwd=2, col=4:1, bty='o')

# Media e Mediana ---------------------------------------------------------

######### Media dados
theta_hat1 <- 1/(sum(dados1$tempo)/sum(dados1$status))
tm1 <- 1/theta_hat1

### Desvio padrao do tempo medio (metodo delta)
var_tm1 <- 1/(sum(dados1$status)*theta_hat1^2)
sd_tm1  <- sqrt(var_tm1)

### IC para o tempo medio
ic_tm1 <- c(tm1 - qnorm(0.975)*sd_tm1, tm1 + qnorm(0.975)*sd_tm1)

######### Media Loco
theta_hat2 <- 1/(sum(Loco$tempo)/sum(Loco$status))
tm2 <- 1/theta_hat2

var_tm2 <- 1/(sum(Loco$status)*theta_hat2^2)
sd_tm2  <- sqrt(var_tm2)

ic_tm2 <- c(tm2 - qnorm(0.975)*sd_tm2, tm2 + qnorm(0.975)*sd_tm2)

######### Media Hemato
theta_hat3 <- 1/(sum(Hemato$tempo)/sum(Hemato$status))
tm3 <- 1/theta_hat3

var_tm3 <- 1/(sum(Hemato$status)*theta_hat3^2)
sd_tm3  <- sqrt(var_tm3)

ic_tm3 <- c(tm3 - qnorm(0.975)*sd_tm3, tm3 + qnorm(0.975)*sd_tm3)

######### Media Mtx
theta_hat4 <- 1/(sum(Mtx$tempo)/sum(Mtx$status))
tm4 <- 1/theta_hat4

var_tm4 <- 1/(sum(Mtx$status)*theta_hat4^2)
sd_tm4  <- sqrt(var_tm4)

ic_tm4 <- c(tm4 - qnorm(0.975)*sd_tm4, tm4 + qnorm(0.975)*sd_tm4)

print(c('Completo', ic_tm1, 'Sólido localizado', ic_tm2, 
        'Metastático', ic_tm4, 'Hematológico', ic_tm3))

######### Mediana

km1 # Completo = 62
km2 # Sólido localizado = NA
km3 # Metastático = 18
km4 # Hematológico = 19.5

print(c('Completo', 62, 'Sólido localizado', NA, 
        'Metastático', 18, 'Hematológico', 19.5))

# Modelos parametricos ----------------------------------------------------

dadosT <- split(dados, dados$gptumor)

DS <- data.frame(gptumor = names(dadosT),
                 N = sapply(dadosT, nrow),
                 Failures = sapply(dadosT, function(x) sum(x$status == 1)),
                 Censored = sapply(dadosT, function(x) sum(x$status == 0)))
rownames(DS) <- NULL

KM <- list()
par(mfrow = c(1, 3))
for(i in 1:3){
  KM[[i]] <- survfit(Surv(tempo, status) ~ 1, data = dadosT[[i]])
  
  plot(KM[[i]], mark.time = T, conf.int = T, col = "red",
       xlim = c(0, 35),
       main = paste0(names(dadosT)[i], " (n = ", nrow(dadosT[[i]]), ")"),
       ylab = expression(S(t)),
       xlab = "Tempo (dias)")
  text(x = 25, y = 0.9, paste0("Media = ", round(sum(summary(KM[[i]])$surv), 2), "\n",
                               "Mediana = ", surv_median(KM[[i]])$median))
}

# Estimate hazard function


RF <- list()
par(mfrow = c(1, 3))
for(i in 1:3){
  RF[[i]] <- muhaz(dadosT[[i]]$tempo, dadosT[[i]]$status, bw.method='local', b.cor='both',
                   min.time = 1, max.time = 182)
  
  plot(RF[[i]], col = "red",
       main = paste0(names(dadosT)[i], " (n = ", nrow(dadosT[[i]]), ")"),
       ylab = expression(h(t)),
       xlab = "Tempo (dias)")
}

# Estimate mean and median survival time


### (c) Fit parametric models:
# Exponential, Gamma, Log-normal, Log-logistic, Gompertz, others


dadosT <- split(dados, dados$gptumor)

tt <- seq(0, 182, 0.1)

par(mfrow = c(1, 2))

nomesTumor <- c("Sólido localizado", 'Metastático', 'Hematológico')

df_survival <- data.frame(Modelo = character(0), AIC = numeric(0), BIC = numeric(0))

for(i in 1:3){
  
  # Exponential
  fit_exp <- fit_data(dadosT[[i]], dist = "exp", time = "tempo", censor = "status")
  ss_exp <- 1 - pexp(tt, fit_exp$estimate)
  hh_exp <- dexp(tt, rate = fit_exp$estimate)/ss_exp
  aic_exp <- summary(fit_exp)$aic
  bic_exp <- summary(fit_exp)$bic
  
  # Weibull
  fit_weib <- fit_data(dadosT[[i]], dist = "weibull", time = "tempo", censor = "status")
  ss_weib <- 1 - pweibull(tt, shape = fit_weib$estimate[1], scale = fit_weib$estimate[2])
  hh_weib <- dweibull(tt,  shape = fit_weib$estimate[1], scale = fit_weib$estimate[2])/ss_weib
  aic_weib <- summary(fit_weib)$aic
  bic_weib <- summary(fit_weib)$bic
  
  # Gamma
  fit_gamma <- flexsurvreg(Surv(dadosT[[i]]$tempo, dadosT[[i]]$status) ~ 1,
                           dist = "gamma")
  ss_gamma <- 1 - pgamma(tt, fit_gamma$res[1], fit_gamma$res[2])
  hh_gamma <- dgamma(tt, fit_gamma$res[1], fit_gamma$res[2])/ss_gamma
  aic_gamma <- AIC(fit_gamma)
  bic_gamma <- BIC(fit_gamma)
  
  # Log-normal
  fit_lnorm <- fit_data(dadosT[[i]], dist = "lnorm", time = "tempo", censor = "status")
  ss_lnorm <- 1 - plnorm(tt, fit_lnorm$estimate[1], fit_lnorm$estimate[2])
  hh_lnorm <- dlnorm(tt, fit_lnorm$estimate[1], fit_lnorm$estimate[2])/ss_lnorm
  aic_lnorm <- summary(fit_lnorm)$aic
  bic_lnorm <- summary(fit_lnorm)$bic
  
  # Log-logistic
  fit_llog <- fit_data(dadosT[[i]], dist = "llogis", time = "tempo", censor = "status")
  ss_llog <- 1 - pllogis(tt, fit_llog$estimate[1], fit_llog$estimate[1])
  hh_llog <- dllogis(tt, fit_llog$estimate[1], fit_llog$estimate[2])/ss_llog
  aic_llog <- summary(fit_llog)$aic
  bic_llog <- summary(fit_llog)$bic
  
  # Gompertz
  fit_gomp <- flexsurvreg(Surv(dadosT[[i]]$tempo, dadosT[[i]]$status) ~ 1,
                          dist = "gamma")
  ss_gomp <- 1 - pgompertz(tt, fit_gomp$res[1], fit_gomp$res[2])
  hh_gomp <- dgompertz(tt, fit_gomp$res[1], fit_gomp$res[2])/ss_gomp
  aic_gomp <- AIC(fit_gomp)
  bic_gomp <- BIC(fit_gomp)
  
  # Generalized Gama
  fit_ggamma <- flexsurvreg(Surv(dadosT[[i]]$tempo, dadosT[[i]]$status) ~ 1,
                            dist = "gengamma.orig")
  ss_ggamma <- 1 - pgengamma.orig(tt, fit_ggamma$res[1], fit_ggamma$res[2], fit_ggamma$res[3])
  hh_ggamma <- dgengamma.orig(tt, fit_ggamma$res[1], fit_ggamma$res[2], fit_ggamma$res[3])/ss_gamma
  aic_ggamma <- AIC(fit_ggamma)
  bic_ggamma <- BIC(fit_ggamma)
  
  # Plot survival function
  plot(KM[[i]], mark.time = F, conf.int = F, col = "black",
       xlim = c(0, 182),
       main = paste0(nomesTumor[i]),
       ylab = expression(S(t)),
       xlab = "Tempo (dias)")
  lines(ss_exp ~ tt, col = "blue")
  lines(ss_weib ~ tt, col = "green")
  lines(ss_gamma ~ tt, col = "purple")
  lines(ss_lnorm ~ tt, col = "yellow")
  lines(ss_llog ~ tt, col = "orange")
  lines(ss_gomp ~ tt, col = "red")
  lines(ss_ggamma ~ tt, col = "darkgreen")
  
  # Plot hazard function
  plot(RF[[i]]$haz.est, type = "l",
       col = "black",
       #ylim = c(0, 1),
       main = paste0(nomesTumor[i]),
       ylab = expression(h(t)),
       xlab = "Tempo (dias)")
  lines(hh_exp[-1] ~ tt[-1], col = "blue")
  lines(hh_weib[-1] ~ tt[-1], col = "green")
  lines(hh_gamma[-1] ~ tt[-1], col = "purple")
  lines(hh_lnorm[-1] ~ tt[-1], col = "yellow")
  lines(hh_llog[-1] ~ tt[-1], col = "orange")
  lines(hh_gomp[-1] ~ tt[-1], col = "red")
  lines(hh_ggamma[-1] ~ tt[-1], col = "darkgreen")
  
  # Adição das informações ao data frame
  df_survival <- rbind(df_survival,
                       data.frame(Modelo = c("Exponencial", "Gama", "Log-normal", "Log-logística", "Gompertz", "Weibull","Gama Generalizada"),
                                  AIC = c(aic_exp, aic_gamma, aic_lnorm, aic_llog, aic_gomp, aic_weib,aic_ggamma),
                                  BIC = c(bic_exp, bic_gamma, bic_lnorm, bic_llog, bic_gomp, bic_weib,bic_ggamma)))
  #pander(df_survival)
}

# Interpretacao ----------------------------------------------------------

# Loco_gom <- a$`gptumor=Loco`
# Hemato_gom <- a$`gptumor=Hemato`
# Mtx_gom <- a$`gptumor=Mtx`
# # 
# # ###### summary(gompertz_fit)$ aplicar
# # 
# # h1 <- muhaz(Loco$tempo, Loco$status, min.time = 1, max.time = 182)
# # 
# # h2 <- muhaz(Hemato$tempo, Hemato$status, min.time = 1, max.time = 182)
# # 
# # h3 <- muhaz(Mtx$tempo, Mtx$status, min.time = 1, max.time = 182)
# # 
# # # Graficos
# # plot(h1, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida',
# #      ylab='Função de Risco Estimada')
# # lines(h3, mark.time=T, conf.int=F, lwd=2, col=2)
# # lines(h2, mark.time=T, conf.int=F, lwd=2, col=3)
# # lines(h0, mark.time=T, conf.int=F, lwd=2, col=4)
# # legend(130,0.025, paste(c('Sólido localizado', 'Metastático', 'Hematológico',
# #                           'Completo')), lwd=2, col=1:4, bty='o')
# 
# plot(km2, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida',
#      ylab='Prob. de sobrevida estimada', col = 4)
# lines(km3, mark.time=T, conf.int=F, lwd=2, col=3)
# lines(km4, mark.time=T, conf.int=F, lwd=2, col=2)
# legend(100,1, paste(c('Sólido localizado', 'Metastático',
#                       'Hematológico')), lwd=1, col=4:2, bty='o')
# 
# lines(Loco_gom$time,Loco_gom$est, lty = 2, col = 4, lwd = 2)
# lines(Hemato_gom$time,Hemato_gom$est, lty = 2, col = 2, lwd = 2)
# lines(Mtx_gom$time,Mtx_gom$est, lty = 2, col = 3, lwd = 2)
# 
# ####################
# # a <- 1 - plnorm(1:182, lognormal_fit$res[1], lognormal_fit$res[2])
# # plot(a)
# 
# loco_a <- 1 - pgompertz(1:182, shape = -0.02722, rate = 0.01851)## Fazer com decimal
# 
# 
# plot(a)

# Comparacao grupos -------------------------------------------------------


dados_comparar <- subset(dados1, gptumor==c('Mtx','Hemato'),
                         select=c('tempo', 'status', 'gptumor'))

Y <- Surv(dados_comparar$tempo, dados_comparar$status)
X <- dados_comparar$gptumor
survdiff(Y~X)

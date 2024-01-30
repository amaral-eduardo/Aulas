dados <- read.table("http://sobrevida.fiocruz.br/dados/ctinca.dat", header = TRUE)
dadosT <- split(dados, dados$gptumor)

DS <- data.frame(gptumor = names(dadosT),
                 N = sapply(dadosT, nrow),
                 Failures = sapply(dadosT, function(x) sum(x$status == 1)),
                 Censored = sapply(dadosT, function(x) sum(x$status == 0)))
rownames(DS) <- NULL

library(survival)
library(survminer)
library(muhaz)
library(parmsurvfit)
library(flexsurv)

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
rm(list = ls())
library(readxl)

# Load data
flower <- read_xlsx("C:\\Git\\Aulas\\AnaliseSobrevivencia\\Trab1\\Matheus\\flower.xlsx", sheet = "data")
flower <- data.frame(flower)

# Remove controls (water and sucrose)
flower <- subset(flower, !family %in% c("0water control", "0sucrose control"))

# Convert into factors
flower[,2:6] <- lapply(flower[,2:6], as.factor)

str(flower)

# Investigate groups
table(flower$year)
table(flower$season)
table(flower$food_species)
table(flower$family)

### (a) Divide according to family
flower <- split(flower, flower$family)

### (b) Descriptive statistics:
# - number of individuals and species in each family
# - number of censored data
DS <- data.frame(Family = names(flower),
                 Nspecies = sapply(flower, function(x) length(unique(x$food_species))),
                 N = sapply(flower, nrow),
                 Failures = sapply(flower, function(x) sum(x$dead == 1)),
                 Censored = sapply(flower, function(x) sum(x$dead == 0)),
                 PropCensored = sapply(flower, function(x) round(1 - mean(x$dead), 4)))
rownames(DS) <- NULL

# Kaplan-Meier estimation for each family (graphs)
library(survival)
library(survminer)

KM <- list()
par(mfrow = c(2, 4))
for(i in 1:8){
  KM[[i]] <- survfit(Surv(time, dead) ~ 1, data = flower[[i]])
  
  plot(KM[[i]], mark.time = T, conf.int = T, col = "red",
       xlim = c(0, 35),
       main = paste0(names(flower)[i], " (n = ", nrow(flower[[i]]), ")"),
       ylab = expression(S(t)),
       xlab = "Tempo (dias)")
  text(x = 25, y = 0.9, paste0("Media = ", round(sum(summary(KM[[i]])$surv), 2), "\n",
                          "Mediana = ", surv_median(KM[[i]])$median))
}



# Estimate hazard function
library(muhaz)

RF <- list()
par(mfrow = c(2, 4))
for(i in 1:8){
  RF[[i]] <- muhaz(flower[[i]]$time, flower[[i]]$dead, bw.method='local', b.cor='both')

  plot(RF[[i]], col = "red",
       main = paste0(names(flower)[i], " (n = ", nrow(flower[[i]]), ")"),
       ylab = expression(h(t)),
       xlab = "Tempo (dias)")
}

# Estimate mean and median survival time


### (c) Fit parametric models:
# Exponential, Gamma, Log-normal, Log-logistic, Gompertz, others
library(parmsurvfit)
library(flexsurv)

tt <- seq(0, 35, 0.1)

x11()
par(mfrow = c(4, 4))

for(i in 1:8){
  
  # Exponential
  fit_exp <- fit_data(flower[[i]], dist = "exp", time = "time", censor = "dead")
  ss_exp <- 1 - pexp(tt, fit_exp$estimate)
  hh_exp <- dexp(tt, rate = fit_exp$estimate)
  
  # Weibull
  fit_weib <- fit_data(flower[[i]], dist = "weibull", time = "time", censor = "dead")
  ss_weib <- 1 - pweibull(tt, shape = fit_weib$estimate[1], scale = fit_weib$estimate[2])
  hh_weib <- dweibull(tt,  shape = fit_weib$estimate[1], scale = fit_weib$estimate[2])
  
  # Gamma
  fit_gamma <- flexsurvreg(Surv(flower[[i]]$time, flower[[i]]$dead) ~ 1,
                           dist = "gamma")
  ss_gamma <- 1 - pgamma(tt, fit_gamma$res[1], fit_gamma$res[2])
  hh_gamma <- dgamma(tt, fit_gamma$res[1], fit_gamma$res[2])
  
  # Log-normal
  fit_lnorm <- fit_data(flower[[i]], dist = "lnorm", time = "time", censor = "dead")
  ss_lnorm <- 1 - plnorm(tt, fit_lnorm$estimate[1], fit_lnorm$estimate[2])
  hh_lnorm <- dlnorm(tt, fit_lnorm$estimate[1], fit_lnorm$estimate[2])
  
  # Log-logistic
  fit_llog <- fit_data(flower[[i]], dist = "lnorm", time = "time", censor = "dead")
  ss_llog <- 1 - pllogis(tt, fit_llog$estimate[1], fit_llog$estimate[1])
  hh_llog <- dllogis(tt, fit_llog$estimate[1], fit_llog$estimate[2])
  
  # Gompertz
  fit_gomp <- flexsurvreg(Surv(flower[[i]]$time, flower[[i]]$dead) ~ 1,
                          dist = "gamma")
  ss_gomp <- 1 - pgompertz(tt, fit_gomp$res[1], fit_gomp$res[2])
  hh_gomp <- dgompertz(tt, fit_gomp$res[1], fit_gomp$res[2])
  
  # Generalized Gama
  fit_ggamma <- flexsurvreg(Surv(flower[[i]]$time, flower[[i]]$dead) ~ 1,
                            dist = "gengamma.orig")
  ss_ggamma <- 1 - pgengamma.orig(tt, fit_ggamma$res[1], fit_ggamma$res[2], fit_ggamma$res[3])
  hh_ggamma <- dgengamma.orig(tt, fit_ggamma$res[1], fit_ggamma$res[2], fit_ggamma$res[3])
  
  # Plot survival function
  plot(KM[[i]], mark.time = F, conf.int = F, col = "black",
       xlim = c(0, 35),
       main = paste0(names(flower)[i], " (n = ", nrow(flower[[i]]), ")"),
       ylab = expression(S(t)),
       xlab = "Tempo (dias)")
  lines(ss_exp, col = "blue")
  lines(ss_weib, col = "green")
  lines(ss_gamma, col = "purple")
  lines(ss_lnorm, col = "yellow")
  lines(ss_llog, col = "orange")
  lines(ss_gomp, col = "red")
  lines(ss_ggamma, col = "darkgreen")
  
  # Plot hazard function
  plot(RF[[i]]$haz.est/max(RF[[i]]$haz.est), type = "l",
       col = "black", ylim = c(0, 1),
       main = paste0(names(flower)[i], " (n = ", nrow(flower[[i]]), ")"),
       ylab = expression(h(t)),
       xlab = "Tempo (dias)")
  lines(hh_exp/max(hh_exp), col = "blue")
  lines(hh_weib/max(hh_weib), col = "green")
  lines(hh_gamma/max(hh_gamma), col = "purple")
  lines(hh_lnorm/max(hh_lnorm), col = "yellow")
  lines(hh_llog/max(hh_llog), col = "orange")
  lines(hh_gomp/max(hh_gomp), col = "red")
  lines(hh_ggamma/max(hh_gamma), col = "darkgreen")
}


### (d) Select best-fitting model for each family and overall

### (e) Interpret results from a biological perspective

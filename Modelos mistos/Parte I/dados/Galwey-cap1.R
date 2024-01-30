# setwd('./Modelos mistos/Parte I/dados')
load("Galwey-tab1-1.RData")
psych::headTail(dat, top = 5, bottom = 5)

attach(dat)


par(mfrow=c(1,3))
hist(price_pounds, main="")
boxplot(price_pounds~town)
plot( latitude,price_pounds )
layout(1)

library(nlme)

# Y_ij = (Mu + Tau_i) [efeito aleatorio da cidade] + BX_i [Efeito fixo] + E_ij
fm1.me <- lme(y ~ latitude , 
              random = ~ 1|town, # Efeito aleatorio no intercepto apenas, devido a cidade 
              method= "REML", data = dat)

summary(fm1.me)


library(rsq)
rsq(fm1.me)


(ef<- ranef(fm1.me))



options(digits = 5)
# valores preditos 
pred<- predict(fm1.me, level = 0:1)  ;pred[1:10, ]


# Estimativa da parte fixa do modelo, no lm4 isto nao funciona, e diferente
(tb<- summary(fm1.me)$tTable)


# Residuos ----------------------------------------------------------------


RE.resp<- residuals(fm1.me, type = "response", level = 0:1); RE.resp[1:10,]


RE.per<- residuals(fm1.me, type = "pearson", level = 0:1)
RE.nor<- residuals(fm1.me, type = "normalized", level = 0:1)




# Resíduos brutos por cidade para o ajuste REML de um 
# modelo de efeitos aleatórios aos dados de preços de 
# residências.

par(mar=c(7,6,1,1), mfrow=c(1,2))
boxplot(RE.resp[,2] ~ town, data = dat,las = 2, ylab = "Resíduos", xlab = "")
abline(h = 0, col="red", lty="dashed")
boxplot(RE.per[,2]  ~ town, data = dat,las = 2, ylab = "Resíduos", xlab = "")
abline(h=c(-3,-2,0,2,3), col=c(3,4,2,4,3),lty=2)


# Resíduos padronizados versus os valores ajustados para o ajuste 
# REML de um modelo de efeitos aleatórios aos dados de preços 
# de residências.
par(mfrow=c(1,3))
plot(pred[,3], RE.resp[,2], xlab = expression(hat(y) ~ " - modelo de efeitos
aleatórios"), ylab = "Resíduo Bruto"  )
abline(h=0, lty=2)
plot(pred[,3], RE.per[,2], ylim = c(-3.5,3.5), xlab = expression(hat(y) ~ " - modelo de efeitos aleatórios"),  ylab = "Resíduo Pearson" )
abline(h=c(-3,-2,0,2,3), col=c(3,2,1,2,3),lty=2)
plot(pred[,3], RE.nor[,2], ylim = c(-3.5,3.5), xlab = expression(hat(y) ~ " - modelo de efeitos aleatórios"),  ylab = "Resíduo Normalizedo" )
abline(h=c(-3,-2,0,2,3), col=c(3,2,1,2,3),lty=2)
layout(1)

library(predictmeans) 
residplot(fm1.me, id = T, newwd = F) ### Forma pratica de ver residuo


par(mfrow=c(1,3))
hist(RE.per[,2], breaks = 10, probability = TRUE)
curve(dnorm(x,0,1), from = -4, to = 4, add = TRUE, col=2)

qqnorm(RE.per[,2]);qqline(RE.per[,2])

library(hnp)
hnp(RE.per[,2], halfnormal = FALSE)



# Resíduos padronizados versus os valores ajustados para 
# o ajuste REML de um modelo de efeitos aleatórios aos 
# dados do experimento ferroviário.

plot(fm1.me)



# Matrizes importantes ----------------------------------------------------

# Matrizes importantes, considerando a cidade de Chichester: 
 
(V <- getVarCov(fm1.me, type = "marginal", individual = "Chichester"))
(D <- getVarCov(fm1.me, type = "random.effects", individual = "Chichester"))
(R_1 <- getVarCov(fm1.me, type="conditional", individual = "Chichester"))



options(digits = 3)

Z <- model.matrix(~town-1,data=dat)

sig2b<- 0.14011^2
sig2 <- 0.13078^2

D   <- diag(11)*sig2b
ZDZ <- Z%*% D %*% t(Z)
R   <- diag(64)*sig2
V   <- ZDZ + R
dim(V)

# cidades:  Chichester, Crewe  e Durham
table(dat$town)
V[29:38, 29:38]

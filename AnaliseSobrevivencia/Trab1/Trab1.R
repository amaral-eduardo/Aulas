# Fonte dos dados ---------------------------------------------------------
# http://sobrevida.fiocruz.br/uti.html
dados <- read.table("http://sobrevida.fiocruz.br/dados/ctinca.dat", header = TRUE)
head(dados)

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

boxplot(dados$idade ~ dados$gptumor)

require(survival)
# dados_sob <- Surv(dados1$tempo, dados1$status)
# 
# km1 <- survfit(dados_sob~1, conf.type='plain')
# summary(km1)
# 
# plot(km1,  mark.time=T, conf.int=T, main = "Completo")
# 
# #### Loco
# Loco_sob <- Surv(Loco$tempo, Loco$status)
# km2 <- survfit(Loco_sob~1, conf.type='plain')
# 
# summary(km2)
# 
# plot(km2,  mark.time=T, conf.int=T, main = "Loco")
# 
# km <- survfit(Loco_sob~1)
# na <- survfit(Loco_sob~1, type='fh')
# plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
# lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
# legend(15,0.3, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')
# 
# 
# #### Mtx
# Mtx_sob <- Surv(Mtx$tempo, Mtx$status)
# km3 <- survfit(Mtx_sob~1, conf.type='plain')
# 
# summary(km3)
# 
# plot(km3,  mark.time=T, conf.int=T, main = "Mtx")
# 
# km <- survfit(Mtx_sob~1)
# na <- survfit(Mtx_sob~1, type='fh')
# plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
# lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
# legend(150,0.8, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')
# 
# #### Hemato
# Hemato_sob <- Surv(Hemato$tempo, Hemato$status)
# km4 <- survfit(Hemato_sob~1, conf.type='plain')
# 
# summary(km4)
# 
# plot(km4,  mark.time=T, conf.int=T, main = "Hemato")
# 
# km <- survfit(Hemato_sob~1)
# na <- survfit(Hemato_sob~1, type='fh')
# plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
# lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
# legend(150,0.8, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')

############### Grafico

(km1 <- survfit(dados_sob~1))
(km2 <- survfit(Loco_sob~1))
(km3 <- survfit(Mtx_sob~1))
(km4 <- survfit(Hemato_sob~1))

plot(km2, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida',
     ylab='Prob. de sobrevida estimada')
lines(km3, mark.time=T, conf.int=F, lwd=2, col=2)
lines(km4, mark.time=T, conf.int=F, lwd=2, col=3)
lines(km1, mark.time=T, conf.int=F, lwd=2, col=4)
legend(130,0.95, paste(c('Sólido localizado', 'Metastático', 'Hematológico',
                         'Completo')), lwd=2, col=1:4, bty='o')

#### 


# Media -------------------------------------------------------------------
######### Media dados
(theta_hat <- 1/(sum(dados$Tempo)/sum(dados$Censura)))
(tm <- 1/theta_hat)

### Desvio padrao do tempo medio (metodo delta)
var_tm <- 1/(sum(dados$Censura)*theta_hat^2)
(sd_tm  <- sqrt(var_tm))

### IC para o tempo medio
(ic_tm <- c(tm - qnorm(0.975)*sd_tm, tm + qnorm(0.975)*sd_tm))


######### Media Loco

######### Media Hemato

######### Media Mtx
(theta_hat <- 1/(sum(dados$Tempo)/sum(dados$Censura)))
(tm <- 1/theta_hat)

### Desvio padrao do tempo medio (metodo delta)
var_tm <- 1/(sum(dados$Censura)*theta_hat^2)
(sd_tm  <- sqrt(var_tm))

### IC para o tempo medio
(ic_tm <- c(tm - qnorm(0.975)*sd_tm, tm + qnorm(0.975)*sd_tm))

# Modelos parametricos ----------------------------------------------------



# Selecao do modelo -------------------------------------------------------


# Interpretacao ----------------------------------------------------------



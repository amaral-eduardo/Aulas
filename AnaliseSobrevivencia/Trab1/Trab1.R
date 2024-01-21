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


# Funcao de sobrevivencia -------------------------------------------------

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

# Funcao de risco ---------------------------------------------------------
require(muhaz)

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
     ylab='Função de Risco Estimada')
lines(h3, mark.time=T, conf.int=F, lwd=2, col=2)
lines(h2, mark.time=T, conf.int=F, lwd=2, col=3)
lines(h0, mark.time=T, conf.int=F, lwd=2, col=4)
legend(130,0.025, paste(c('Sólido localizado', 'Metastático', 'Hematológico',
                         'Completo')), lwd=2, col=1:4, bty='o')




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



# Selecao do modelo -------------------------------------------------------


# Interpretacao ----------------------------------------------------------



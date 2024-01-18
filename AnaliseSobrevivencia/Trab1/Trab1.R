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

str(dados)

# Escolha da variavel -----------------------------------------------------
# Variavel explicativa para separar os dados em 3 grupos

Loco <- subset(dados, gptumor=="Loco", select=c('tempo', 'status'))
Mtx <- subset(dados, gptumor=="Mtx", select=c('tempo', 'status'))
Hemato <- subset(dados, gptumor=="Hemato", select=c('tempo', 'status'))

dados1 <- dados[,c('tempo', 'status', 'gptumor')]

# Analise descritiva ------------------------------------------------------

boxplot(dados$idade ~ dados$gptumor)

# Modelos parametricos ----------------------------------------------------
require(survival)
dados_sob <- Surv(dados1$tempo, dados1$status)

km1 <- survfit(dados_sob~1, conf.type='plain')
summary(km1)

plot(km1,  mark.time=T, conf.int=T, main = "Completo")

#### Loco
Loco_sob <- Surv(Loco$tempo, Loco$status)
km2 <- survfit(Loco_sob~1, conf.type='plain')

summary(km2)

plot(km2,  mark.time=T, conf.int=T, main = "Loco")

km <- survfit(Loco_sob~1)
na <- survfit(Loco_sob~1, type='fh')
plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
legend(15,0.3, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')


#### Mtx
Mtx_sob <- Surv(Mtx$tempo, Mtx$status)
km3 <- survfit(Mtx_sob~1, conf.type='plain')

summary(km3)

plot(km3,  mark.time=T, conf.int=T, main = "Mtx")

km <- survfit(Mtx_sob~1)
na <- survfit(Mtx_sob~1, type='fh')
plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
legend(150,0.8, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')

#### Hemato
Hemato_sob <- Surv(Hemato$tempo, Hemato$status)
km4 <- survfit(Hemato_sob~1, conf.type='plain')

summary(km4)

plot(km4,  mark.time=T, conf.int=T, main = "Hemato")

km <- survfit(Hemato_sob~1)
na <- survfit(Hemato_sob~1, type='fh')
plot(km, mark.time=T, conf.int=F, lwd=2, xlab='Tempo de sobrevida', ylab='Prob. de sobrevida estimada')
lines(na, mark.time=T, conf.int=F, lwd=2, col=2)
legend(150,0.8, paste(c('Kaplan-Meier', 'Nelson-Aalen')), lwd=2, col=1:2, bty='o')
# Selecao do modelo -------------------------------------------------------


# Interpretacao ----------------------------------------------------------



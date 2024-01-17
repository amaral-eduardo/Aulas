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



# Modelos parametricos ----------------------------------------------------



# Selecao do modelo -------------------------------------------------------



# Interpretacao ----------------------------------------------------------



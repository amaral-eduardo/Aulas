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

summary(dados)

# Escolha da variavel -----------------------------------------------------



# Analise descritiva ------------------------------------------------------



# Modelos parametricos ----------------------------------------------------



# Selecao do modelo -------------------------------------------------------



# Interpretacao ----------------------------------------------------------



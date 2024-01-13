# http://sobrevida.fiocruz.br/tmo.html

setwd("./AnaliseSobrevivencia")
dados2 <- read.csv("tmopc.csv", sep = ";")
dados <- read.table("dados.txt", header = T)
head(dados)

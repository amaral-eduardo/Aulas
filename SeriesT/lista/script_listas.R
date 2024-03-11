# setwd('./SeriesT/lista')
require(readxl)
dados1 <- read_xlsx('2.xlsx')

dados1 <- as.ts(dados1)
is.ts(dados1)

autoplot(dados1)


# Modelos para comparação
exp_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "exponential")

gama_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "gamma")

lognormal_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "lognormal")

loglogistic_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "llogis")

gompertz_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "gompertz")

weibull_fit = flexsurvreg(Surv(tempo, status) ~ gptumor, data = dados, dist = "weibull")


################################ DEF analisar_residuos ############################################
analisar_residuos <- function(ajuste, dados, distribuicao) {
  # Laço de iteração para calcular estimativas e gerar resíduos simulados
  s <- c()
  
  for(i in 1:nrow(dados)) {
    s[i] <- summary(ajuste, type='survival', t=dados$tempo[i], newdata=dados[i,], tidy=TRUE)$est
    #if(i %% 10 == 0) print(i)
  }
  
  # Geração de resíduos simulados
  a <- runif(length(s), min=0, max=s)
  r <- ifelse(dados$status == 1, qnorm(s), qnorm(a))
  
  # Geração do gráfico HNP
  Grap = hnp(r, plot.sim = F, main = paste("HNP"))
  G <- with(Grap, data.frame(x, lower, upper, median, residuals))
  
  grafico_hnp <- ggplot(data = G, aes(x)) +
    geom_point(aes(y = residuals), color = "black", shape = 1) +
    geom_line(aes(y = lower), color = "black") +
    geom_line(aes(y = upper), color = "black") +
    geom_line(aes(y = median), color = "black", linetype = "dashed") + 
    ylab("Resíduos") +
    xlab("Quantis Teóricos") +
    ggtitle(paste0("HNP"," - ",distribuicao)) + 
    theme_minimal() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, size = 1))
  print(paste0("HNP"," - ",distribuicao,"\n"))
  print(grafico_hnp)
  
}

#########################################################################################

nomes <- c("Exponencial", "Gama", "Log-normal", "Log-logística", "Gompertz", "Weibull")
modelos <- list(exp_fit, gama_fit, lognormal_fit, loglogistic_fit, gompertz_fit, weibull_fit)

for (i in 1:length(nomes)) {
  analisar_residuos(modelos[[i]], dados, nomes[i])
}


######### TABELA AIC BIC #######################################
comparar_modelos <- function(lista_de_modelos, nomes_modelos) {
  # Verifica se o comprimento de nomes_modelos é igual ao comprimento da lista_de_modelos
  if (length(nomes_modelos) != length(lista_de_modelos)) {
    stop("O número de nomes de modelos não corresponde ao número de modelos na lista.")
  }
  
  # Inicializa um data frame vazio
  resultados <- data.frame(Modelo = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
  
  # Loop sobre a lista de modelos
  for (i in seq_along(lista_de_modelos)) {
    modelo <- lista_de_modelos[[i]]
    
    # Calcula AIC e BIC
    aic_valor <- AIC(modelo)
    bic_valor <- BIC(modelo)
    
    # Adiciona os resultados ao data frame
    resultados <- rbind(resultados, data.frame(Modelo = nomes_modelos[i], AIC = aic_valor, BIC = bic_valor))
  }
  
  # Retorna a tabela de resultados
  return(resultados)
}

#######################################################################

# USO
nomes <- c("Exponencial", "Gama", "Log-normal", "Log-logística", "Gompertz", "Weibull")
modelos <- list(exp_fit, gama_fit, lognormal_fit, loglogistic_fit, gompertz_fit, weibull_fit)

tabela_resultados <- comparar_modelos(modelos, nomes)
pander(tabela_resultados)




knitr::kable(lognormal_fit$res.t,align = "c")








ravioli <- transform(ravioli,
                     day = as.factor(day),
                     presentation = as.factor(presentation),
                     serving = as.factor(serving),
                     brand = as.factor(brand),
                     assessor = as.factor(assessor))

str(ravioli)

boxplot(saltiness ~ brand*assessor, data = ravioli)

fm1 <- aov(saltiness ~ brand*assessor + Error(day/presentation/serving),
           data = ravioli)
summary(fm1)

## Estimar os valores pela pagina 623 do livro (Tabela)

library(dae)
res2 <- resid.errors(fm1)
fit <- fitted(fm1)

qqnorm(res2); qqline(res2)


plot(fit, res2, xlab = expression(hat(log(y))), ylab = 'Residuos')

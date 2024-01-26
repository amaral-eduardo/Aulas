# data set
# https://www2.compute.dtu.dk/courses/02429/Data/

planks<-read.table("planks.txt",header=T,sep =",")


planks<- transform(planks, plank=factor(plank),
                   width=factor(width),
                   depth=factor(depth),
                   loghum=log(humidity))
attach(planks)

psych::headTail(planks, 2,4)


par(mfrow=c(2,2))
with(planks, interaction.plot(width,plank,humidity,legend=F,col=hsv(1:20/20)))
with(planks, interaction.plot(depth,plank,humidity,legend=F,col=hsv(1:20/20)))
with(planks, interaction.plot(width,depth,humidity,legend=T,col=hsv(1:5/5), lwd=2))
with(planks, interaction.plot(depth,width,humidity,legend=T,col=hsv(1:3/3), lwd=2))
# par(op)





# Tabela ANOVA da análise em que efeitos depth*width estão aninhados em plank:

fm.nested0<- aov( humidity ~ depth*width + Error(plank/(depth*width)), data = planks)
summary(fm.nested0)


X11(title = "humidity")
par(mfrow=c(2,2))
plot(resid(fm.nested0[[2]]) ~ fitted(fm.nested0[[2]]))
plot(resid(fm.nested0[[3]]) ~ fitted(fm.nested0[[3]]))
plot(resid(fm.nested0[[4]]) ~ fitted(fm.nested0[[4]]))
plot(resid(fm.nested0[[5]]) ~ fitted(fm.nested0[[5]]))
layout(1)

length(resid(fm.nested0[[2]]))

res<- c(
  (fm.nested0[[2]]$ residuals),
  (fm.nested0[[3]]$ residuals),
  (fm.nested0[[4]]$ residuals),
  (fm.nested0[[5]]$ residuals))

qqnorm(res)
qqline(res)


# vamos ajustar um modelo completo considerando todos os efeitos 
# fixos para entender  o problema de ajuste



# Tabela ANOVA da análise em que efeitos todos os efeitos são considerados fixos,
# isto é, apenas o ultimo erro do modelo:

model1.aov <- aov(humidity ~ depth*width + depth*plank + plank*width, data = planks)
summary(model1.aov)

X11()
par(mfrow=c(2,2))
  plot(model1.aov)
layout(1)
hnp::hnp(model1.aov, halfnormal = F)


library(MASS)
ob<- boxcox(model1.aov)


(lambda<- ob$x[ which.max(ob$y)])

# usaremos ln de humidity  -> loghum


model2.aov <- aov(loghum ~ depth*width + depth*plank + plank*width, data = planks)
summary(model2.aov)

X11()
par(mfrow=c(2,2))
  plot(model2.aov)
layout(1)



library(MASS)
boxcox(model2.aov)



# usaremos ln de humidity  -> loghum



# Tabela ANOVA da análise em que efeitos depth*width estão aninhados em plank:

fm.nested<- aov( loghum ~ depth*width + Error(plank/(depth*width)), data = planks)
summary(fm.nested)



X11(title = "loghum")
par(mfrow=c(2,2))
plot(resid(fm.nested[[2]]) ~ fitted(fm.nested[[2]]))
plot(resid(fm.nested[[3]]) ~ fitted(fm.nested[[3]]))
plot(resid(fm.nested[[4]]) ~ fitted(fm.nested[[4]]))
plot(resid(fm.nested[[5]]) ~ fitted(fm.nested[[5]]))
layout(1)


res<- c(
  (fm.nested[[2]]$ residuals),
  (fm.nested[[3]]$ residuals),
  (fm.nested[[4]]$ residuals),
  (fm.nested[[5]]$ residuals))

qqnorm(res)
qqline(res)



# Modelo misto considerando plank aleatório
library(lmerTest)
library(lme4)

model4.o<-lmer(humidity ~ depth*width + (1|plank) + (1|depth:plank)
             + (1|plank:width), data = planks)

model4<-lmer(loghum ~ depth*width + (1|plank) + (1|depth:plank)
             + (1|plank:width), data = planks)

summary(model4)

# diagnóstico do modelo
library(predictmeans)
residplot(model4.o, id = TRUE, newwd=TRUE)
residplot(model4, id = TRUE, newwd=TRUE)



# Residuals
plot(model4, plank~resid(., type ="pearson"),ylab="plank",     
     xlab="Res. de Pearson", abline = 0 ,id=0.0005)
plot(model4, depth~resid(., type ="pearson"),ylab="depth",     
     xlab="Res. de Pearson", abline = 0 ,id=0.0005)
plot(model4, width~resid(., type ="pearson"),ylab="width",     
     xlab="Res. de Pearson", abline = 0 ,id=0.0005)

plot( model4, resid(., type ="pearson") ~ fitted(.), ylab="Res. de Pearson",     
      xlab="Valores ajustados", 
      id = 0.0005, adj = -0.3, subtit="", mar=c(.6,2,1,1), pch=16,
      ylim = c(-2.5,2.5), abline=c(-2,0,2), lty=2)



# Resultados do ajuste
(tb<- summary(model4))

# teste da parte fixa do modelo
(tb2<- anova(model4))



tb$coefficients
tb$varcor
tb$sigma

VarCorr(model4)
 # Groups      Name        Std.Dev.
 # depth:plank (Intercept) 0.024635
 # plank:width (Intercept) 0.090464
 # plank       (Intercept) 0.169805
 # Residual                0.070238

# matrizes do modelo
X  <- getME(model4,"X")
Z  <- getME(model4,"Z")
Z2 <- getME(model4,"Ztlist")

X<- model.matrix(model4)

betas  <- getME(model4,"beta")




## Médias de mínimos quadrados e diferenças entre pares:

(lsm <- ls_means(model4))
plot(lsm, which=c("depth","width"))


## Diferenças pareadas caso a interação
## não fosse significativa

(dd<- ls_means(model4, which = "depth", pairwise = TRUE))
(ww<- ls_means(model4, which = "width", pairwise = TRUE))

plot(dd)
plot(ww)

(dw<- ls_means(model4, which = "depth:width", pairwise = TRUE))
plot(dw)

#                                 Estimate  Std. Error    df  t value       lower       upper  Pr(>|t|)    
# depth1:width1 - depth3:width1 -0.25134902  0.02353778 222.7 -10.6785 -0.29773434 -0.20496369 < 2.2e-16 ***
# depth1:width1 - depth5:width1 -0.31888470  0.02353778 222.7 -13.5478 -0.36527002 -0.27249937 < 2.2e-16 ***  
# depth1:width1 - depth7:width1 -0.27316203  0.02353778 222.7 -11.6053 -0.31954736 -0.22677671 < 2.2e-16 ***
# depth1:width1 - depth9:width1 -0.02964715  0.02353778 222.7  -1.2596 -0.07603248  0.01673818 0.2091483    
# depth3:width1 - depth5:width1 -0.06753568  0.02353778 222.7  -2.8692 -0.11392101 -0.02115035 0.0045105 ** 
# depth3:width1 - depth7:width1 -0.02181302  0.02353778 222.7  -0.9267 -0.06819834  0.02457231 0.3550735    
# depth3:width1 - depth9:width1  0.22170187  0.02353778 222.7   9.4190  0.17531654  0.26808719 < 2.2e-16 ***
# depth5:width1 - depth7:width1  0.04572266  0.02353778 222.7   1.9425 -0.00066266  0.09210799 0.0533349 .  
# depth5:width1 - depth9:width1  0.28923755  0.02353778 222.7  12.2882  0.24285222  0.33562287 < 2.2e-16 ***
# depth7:width1 - depth9:width1  0.24351488  0.02353778 222.7  10.3457  0.19712956  0.28990021 < 2.2e-16 ***
  
  
  
# média e IC em cada um dos 15 tratamentos  
library(emmeans)
(dw<- lsmeans(model4, ~ depth*width))


plot(dw)


library(lmerTest)
mydifflsmeans<- difflsmeans(model4,"depth:width")
head(mydifflsmeans,10) 
hist(mydifflsmeans$`Std. Error`)


# O histograma mostra que existem dois níveis de erros: 
# um para comparar larguras dentro de profundidades e 
# outro para comparar profundidades dentro de larguras.
browseURL("http://www2.compute.dtu.dk/courses/02429/enotepdfs/eNote-6.pdf") # pag. 34
plot(model4, which=c("depth:width") )






# comparando níveis de "depth" dentro de  "width"
# Usando as informações do aov e fazendo manualmente


# Usando emmeans
pairs(emmeans(fm.nested , ~depth|width, adjust = "tukey"))

# Usando o modelo misto
pairs(emmeans(model4 , ~depth|width))




# comparando níveis de "width" dentro de  "depth"
# Usando as informações do aov e fazendo manualmente

# Usando emmeans

library(emmeans)
pairs(emmeans(fm.nested , ~width|depth))
pairs(emmeans(fm.nested , ~depth|width))

# Usando o modelo misto
pairs(emmeans(model4 , ~width|depth))
pairs(emmeans(model4 , ~depth|width))



library(predictmeans)
predictmeans(model4, "depth:width",atvar="depth" , pairwise=TRUE, adj="tukey")
predictmeans(model4, "depth:width",atvar="width" , pairwise=TRUE, adj="tukey")

#=======================================================================================
# Os códigos a seguir são para estética de 
# relatórios

w1<- "https://m-clark.github.io/mixedup/index.html"
browseURL(w1)

remotes::install_github('m-clark/mixedup')

# if you don't already have rstanarm and/or brms

withr::with_envvar(c(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true"), 
                   remotes::install_github('m-clark/mixedup')
)

library(mixedup)
extract_random_effects(model4)

extract_vc(model4)

summarize_model(model4, cor_re = T, digits = 4)


extract_variance_components(model4)
extract_VarCorr(model4)

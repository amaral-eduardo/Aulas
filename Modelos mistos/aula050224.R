library(nlme)
psych::headTail(Orthodont)

Orthodont$id<- as.factor(rep(1:27, each=4))

library(latticeExtra)

xyplot(distance ~ age | Sex, data=Orthodont, type="b", groups = Subject, pch=19)

plot(Orthodont)

###############################
lm.Ort <- lm(distance ~ age + Sex + age:Sex, data = Orthodont)
summary(lm.Ort)
## 16.3406 + 0.7844 * Age+ + 0.7844 * SexF - 0.3048 * AgeF


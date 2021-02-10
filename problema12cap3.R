setwd("C:/Users/YURIELA/Desktop/Diseño Expirimental")
 

df=read.csv("problema cap 3-12.csv")

str(df)
df$tratamiento=factor(df$tratamiento)

boxplot(Tiempo~tratamiento,data=df,main="COOK TIME")

modelo=aov(Tiempo~tratamiento,data = df)
summary(modelo)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

library(car)

leveneTest(df$Tiempo~df$tratamiento)

plot(modelo$residuals)
abline(h=0)

plot(df$tratamiento,modelo$residuals)
abline(h=0)

plot(modelo$fitted.values,modelo$residuals)
abline(h=0)

            
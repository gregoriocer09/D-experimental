setwd("C:/Users/GREG/Desktop/Diseno Experimental")

df=read.csv("Cap3Prob12.csv")
df

str(df)
df$tratamiento=factor(df$tratamiento)

boxplot(Tiempo~tratamiento,data=df,main="comparar los tiempos de coccion con diferentes tratamientos")

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
setwd("C:/Users/GREG/Desktop/Diseno EXPERIMENTAL")

df=read.csv("Sprays.csv")
df

df$Marca=factor(df$Marca)
str(df)

boxplot(Y~Marca,data=df)

modelo=aov(Y~Marca,data=df)
summary(modelo)

1-pf(2.793,2,15)

tk=TukeyHSD(modelo)
tk

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library("car")
leveneTest(Y~Marca,data=df)


plot(modelo$residuals)
abline(h=0)

plot(modelo$fitted.values,modelo$residuals)
abline(h=0)

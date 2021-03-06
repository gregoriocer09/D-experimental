setwd("C:/Users/GREG/Desktop/Diseno Experimental")

df=read.csv("Cap3Prob14.R.csv")
df

str(df)
df$Trat=factor(df$Trat)

boxplot(PPD~Trat,data=df)

boxplot(PPD~Trat,data=df,main="Tratamientos de Productos Defectuosos")

modelo=aov(PPD~Trat,data = df)
summary(modelo)

tk=TukeyHSD(modelo)
tk
plot(tk)

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

library(car)

leveneTest(df$PPD~df$Trat)

plot(modelo$residuals)
abline(h=0)

plot(df$PPD,modelo$residuals)
abline(h=0)
plot(modelo$fitted.values,modelo$residuals)
abline(h=0)





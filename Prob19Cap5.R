df=read.csv("Prob21Cap5.csv")
df

str(df)
df$Acelerante=factor(df$Acelerante)
df$TC=factor(df$TC)
df$Y=as.double(df$Y)

modelo=aov(Y~Acelerante+TC,data=df)
summary(modelo)

boxplot(Y~TC,data=df)
boxplot(Y~Acelerante,data=df)
boxplot(Y~Acelerante*TC,data=df)
interaction.plot(df$Acelerante,df$TC,df$Y,main="Interccion entre las variables")

tk=TukeyHSD(modelo)

qqnorm(modelo$residuals)
qqline(modelo$residuals)
shapiro.test(modelo$residuals)

require(car)
leveneTest(Y~Acelerante,data=df)
leveneTest(Y~TC,data=df)

plot(modelo$residuals)
abline(h=0)
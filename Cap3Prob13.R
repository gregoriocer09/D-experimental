T20=c(15,18,13,12)
T40=c(17,21,11,16)
T60=c(23,19,25,22)
T80=c(28,32,34,31)
T100=c(45,51,57,48)


df=data.frame(T20=T20,T40=T40,T60=T60,T80=T80,T100=T100)
df=stack(df)
df

names(df)=c("Y","TEMPERATURA")
df

boxplot(Y~TEMPERATURA,data=df)


modelo=aov(Y~TEMPERATURA,data=df)
summary(modelo)

tk=TukeyHSD(modelo)
tk
plot(tk)

1-pf(68.06,4,15)


qqnorm(modelo$residuals,col="pink",main="Prueba de Temperatura para tableros de carros")
qqline(modelo$residuals,col="purple",lwd=2)

shapiro.test(modelo$residuals)



library("car")
leveneTest(df$Y~df$TEMPERATURA,data=df)

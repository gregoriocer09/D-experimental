df=read.csv("Prob15Cap4.csv",header = TRUE)
df

df$Silo=factor(df$Silo)
df$Dia=factor(df$Dia)
df$Y=as.double(df$Y)
str(df)

# a) Observe los datos y establezca una conjetura acerca de la confiabilidad de las mediciones con Rtd (del termometro de mercurio no hay duda)
# b) Es claro que el silo se puede ver como tratamiento y dias como bloque. Considere
# solo los datos de Rtd y establezca el modelo estadistico. Tambien haga el ANOVA
# correspondiente y obtenga conclusiones.
  
modelo=aov(Y~Silo+Dia,data=df)
summary(modelo)
boxplot(Y~Silo,data=df)
boxplot(Y~Dia,data=df)

# R/. Silo: Los silos no son iguales ya que presentan un valor de P<0.05: 
# Hipotesis alternativa.
# Dia:Los dias tienen temperaturas iguales ya que presentan un valor 
# de P>0.05: Hipotesis nula.
# R/. Anova: Analisis Propio.
# Adicional (no lo pide el problema):

tk=TukeyHSD(modelo)
tk
plot(tk)
# Con la prueba Tukey se observa las diferencia entre Silos y Dias

qqnorm(modelo$residuals)
qqline(modelo$residuals)
# Analisis de supuestos: Los datos vienen de una distribucion normal.

shapiro.test(modelo$residuals)

# Prueba de Shapiro: P>0.05 se acepta H0.

# c) Repita el inciso anterior pero ahora para las mediciones Mer.

dt=read.csv("Prob15Cap4.csv",header = TRUE)
dt

dt$Silo=factor(dt$Silo)
dt$Dia=factor(dt$Dia)
dt$Y=as.double(dt$Y)
str(dt)

modelo=aov(Y~Silo+Dia,data=dt)
summary(modelo)
boxplot(Y~Silo,data=dt)
boxplot(Y~Dia,data=dt)
tk=TukeyHSD(modelo)
tk
plot(tk)
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Analisis de lo obtenido: Silo: P>0.05: Hipotesis nula
#                          Dias: P>0.05: Hipotesis nula

# d)¿Las conclusiones obtenidas en los incisos anteriores coinciden? Comente su respuesta.
# Analisis propio.

# e) Datos pareados. Para comparar los dos mÃ©todos de medicion (Mer y Rtd) obtenga
# como variable de respuesta a la diferencia de temperatura que registran los metodos 
# para cada dia en cada silo. Considerando esto, establezca el modelo estadistico, haga el ANOVA correspondiente y obtenga conclusiones.

dp=read.csv("Prob15Cap4.csv",header = TRUE)
dp

dp$Silo=factor(dp$Silo)
dp$Dia=factor(dp$Dia)
dp$Y=as.double(dp$Y)
str(dp)

modelo=aov(Y~Silo+Dia,data=dp)
summary(modelo)
boxplot(Y~Silo,data=dp)
boxplot(Y~Dia,data=dp)
tk=TukeyHSD(modelo)
tk
plot(tk)
qqnorm(modelo$residuals)
qqline(modelo$residuals)

# Analisis de lo obtenido: Silo: P<0.05: Hipotesis alternativa
#                          Dias: P>0.05: Hipotesis nula

Mujer=c(75,77,78,79,77,73,78,79,78,80)
Hombre=c(74,72,77,76,76,73,75,73,74,75)

t.test(Mujer,Hombre)

df=data.frame(Mujer=Mujer,Hombre=Hombre)
df=stack(df)
df

names(df)=c("T","Genero")
df

str(df)

boxplot(T~Genero,data=df)






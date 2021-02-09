Tbaja=c(17.2,17.5,18.6,15.9,16.4,17.3,16.8,18.4,16.7,17.6)
Talta=c(21.4,20.9,19.8,20.4,20.6,21.0,20.8,19.9,21.1,20.3)

t.test(Tbaja,Talta)

df=data.frame(Tbaja=Tbaja,Talta=Talta)
df=stack(df)
df


names(df)=c("T","Temperatura")
df

str(df)

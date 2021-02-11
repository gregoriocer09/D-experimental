x=c(1.81,1.97,1.93,1.97,1.85,1.99,1.95,1.93,1.85,1.87,1.98,1.93,1.96,2.02,2.07,1.92,1.99,1.93)
n=length(x)
n

media=mean(x)
std=sd(x)
media
std


hist(x,freq=FALSE,col="blue")


tmin=qt(0.005,n-1)
tmax=qt(0.995,n-1)


tmin
tmax

media+tmin*std/sqrt(n)
media+tmax*std/sqrt(n)


(n-1)*var(x)/qchisq(0.005,n-1)
(n-1)*var(x)/qchisq(0.995,n-1)

boxplot(x)
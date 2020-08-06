
scaler<-1


q1<-5*scaler
q2<-12*scaler
q3<-26*scaler

fn<-function(p) (q1-qgamma(0.25,shape=p[1],rate=p[2]))^2+(q2-qgamma(0.5,shape=p[1],rate=p[2]))^2+(q3-qgamma(0.75,shape=p[1],rate=p[2]))^2

out<-optim(c(8,1),fn,lower=c(0.00001,0.00001),upper=c(Inf,Inf),method="L-BFGS-B")

params<-out$par

qgamma(0.25,shape=params[1],rate=params[2])
qgamma(0.5,shape=params[1],rate=params[2])
qgamma(0.75,shape=params[1],rate=params[2])

params[1]/params[2]

params

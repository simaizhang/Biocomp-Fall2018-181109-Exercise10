# 1
setwd("~/bio/exercise/Biocomp-Fall2018-181109-Exercise10/")
data=read.table('data.txt',header=TRUE,sep=',')

# maximum likelihood for linear model
nllike<-function(p,x,y){
  B0=p[1] 
  B1=p[2] 
  sigma=exp(p[3])
  expected=B0+B1*x
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}
initial1=c(1,1,1)
fitSimple=optim(par=initial1,fn=nllike,x=data$x,y=data$y)
print(fitSimple)

# maximum likelihood for quadratic model
nllike2<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  sigma=exp(p[4])
  expected=B0+B1*x+B2*x^2
  nll=-sum(dnorm(x=y,mean=expected,sd=sigma,log=TRUE))
  return(nll)
}
initial2=c(1,1,1,1)
fitComplex=optim(par=initial2,fn=nllike,x=data$x,y=data$y)
print(fitComplex)

#  use Likelihood Ratio Test to evaluate two models 
#  Testing for statistical significance with maximum likelihood
teststat=2*(fitSimple$value-fitComplex$value)
df=length(fitComplex$par)-length(fitSimple$par)
pValue=1-pchisq(teststat,df=df)
if (pValue<0.05)
 { print("quadratic model is better") }
if (pValue>0.05)
 { print("No need to use quadratic model, linear model is better")}


# 2
ddSim<-function(t,y,p){
  N1=y[1]
  N2=y[2]
  r1=p[1]
  r2=p[2]
  a11=p[3]
  a12=p[4]
  a21=p[5]
  a22=p[6]
  dN1dt=r1*(1-N1*a11-N2*a12)*N1
  dN2dt=r2*(1-N2*a22-N1*a21)*N2
  return (list(c(dN1dt,dN2dt)))
}

# all a value < 0.01
  times=1:100
  # 1st model simulation
  params1=c(0.5,0.5,0.003,0.001,0.007,0.009)
  modelSim=ode(y=y0,times=times,func=ddSim,parms=params1)
  # 2nd model simulation
  params2=c(0.5,0.5,0.005,0.004,0.008,0.009)
  modelSim=ode(y=y0,times=times,func=ddSim,parms=params2)
  # 3rd model simulation 
  params3=c(0.5,0.5,0.009,0.008,0.003,0.002)
  modelSim=ode(y=y0,times=times,func=ddSim,parms=params3)
  # the appropriate model simulation
  params4=c(0.5,0.5,0.01,0.007,0.008,0.01)
  modelSim=ode(y=y0,times=times,func=ddSim,parms=params4)
  
  modelOutput=data.frame(time=modelSim[,1],species1=modelSim[,2],species2=modelSim[,3])
  plot=ggplot(data=modelOutput) + xlab("time")+ ylab("species")+geom_line(data=modelOutput,aes(x=time,y=species1),col='blue')+geom_line(data=modelOutput,aes(x=time,y=species2),col='red')+theme_classic()
  plot
  
  
                                                                                                        
                                                                                                        
  
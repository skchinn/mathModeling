### LESSON 10 ###
library(MASS);library(NlcOptim);library(ma391chinn)
### Problem 1 ###
P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}
x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
B = matrix(c(5000,8000,10000,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)

f = function(a){
  P = function(x){
    return(((339-a*x[1]-0.003*x[2])*x[1]
            +(399-0.004*x[1]-0.01*x[2])*x[2]
            -(400000+195*x[1]+225*x[2]))*-1)
  }
  x0=c(1000,1000)
  A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
  B = matrix(c(5000,8000,10000,0,0),nrow=5)
  ans = solnl(x0,objfun=P,A=A,B=B)
  return(ans)
}
ans.x1=0
ans.x2=0
ans.profit=0
sa = seq(0.001,0.025,0.001)
for (i in 1:length(sa)){
  res = f(sa[i])
  ans.x1[i]=res$par[1]
  ans.x2[i]=res$par[2]
  ans.profit[i]= -res$fn
}
result = data.frame(a = sa,x1=ans.x1,x2=ans.x2,profit = ans.profit)
print(result)

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}

x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) 
B = matrix(c(5000,8000,10001,0,0),nrow=5)
print(A)
print(B)
solnl(x0,objfun=P,A=A,B=B)

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}

x0=c(1000,1000)
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5) 
B = matrix(c(3000,8000,10000,0,0),nrow=5)
solnl(x0,objfun=P,A=A,B=B)

### Problem 2 ###

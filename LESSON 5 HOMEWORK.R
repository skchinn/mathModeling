### LESSON 5 ###
library(ma391chinn)

# x = # in species x population (blue whale)

dX = function(x){.05*x*(1-(x/150000))-((10^-8)*x*y)}
dY = function(x){.08*y*(1-(y/400000))-((10^-8)*x*y)}

G = function(x){(.05*x*(1-(x/150000))-(a*x*y))+
    (.08*y*(1-(y/450000))-(a*x*y))}
dGdX = fprime(G,x)
dGdY = fprime(G,y)

x=0
G(0)
bisection(G,0,100000)
print(G(69103.7))

P = function(r1){
  f = function(x){(r1*x*(1-(x/150000))-((10^-8)*x*y))+
      (.08*y*(1-(y/400000))-((10^-8)*x*y))*-1
  }
  x = c(69103,196544)
  ans = optim(x,f,method="BFGS")
  ## Choose which of these that we will return ##
  print(ans$par)
  print(ans$value)   
}
P(0.05)

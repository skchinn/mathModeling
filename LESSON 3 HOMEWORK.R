### LESSON 3 ###
### Problem 4 - Oil ###
# (a)
library(ma391chinn)

#Variables/Facts:
 
# 200 mile oil spill to clean up
# $x$ = additional crews to hire
# $d$ = time it takes to clean up (days)
# $r$ = clean-up rate for a crew (miles/day)
# $C_1$ = cost of local cleanup crew (dollars) = $500d$
# $C_2$ = cost of $x$ additional crews (dollars) = $(18000+800d)x$
# $C_fine$ = cost of fines after 14 days = $10000(d-14)$ if d>14
# $C$ = total cost of clean-up = $C_1 + C_2 + C_fine$

#Assumptions:
  
# days to complete clean-up are not affected by anything other than the number of crews and the rate (weather, etc): $d = \frac{200}{r(x+1)}$
# cost for fines are only levied after 14 days - becomes a step function
# all of the crews clean-up the spill at the same rate as the local crew $r - 5/$ (miles/day)

## Define the function using boolean statements such as >, <=, == 
# Given: 5 miles of beach per week
r = 5/7 
# Define the function
fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))} 
# Define the derivative
dfT = function(x){fprime(fT,x)}
# Define the domain to graph on
x = seq(0,20,1)
# Plot the function
plot(x,fT(x),type="o")

fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
ans = bisection(dfT,0,20)
print(ans)

# Maximum number of crews is 11 (from 11.28).

print(fT(ans))

# The total clean-up cost will be $508,212.60.

# (b)
ans.crew=0
ans.cost=0
ans.cost1 <- na.omit(ans.cost)
r = seq(2/7,8/7,1/7)
for (i in 1:length(r)){
  ## Change r to the variable r[i] to iterate the changing rate
  fT = function(x){return(500*(200/(r[i]*(x+1)))+(18000+800*200/(r[i]*(x+1)))*x+(200/(r[i]*(x+1))>14)*(10000*(200/(r[i]*(x+1))-14)))}
  dfT = function(x){fprime(fT,x)}
  ans.crew[i] = bisection(dfT,0,20)
  ans.cost1[i] = fT(ans.crew[i])
}
result = data.frame(rate = r, crew = ans.crew, cost = ans.cost1)
print(result)

#As rate increases from 2/7 to 8/7 the amount of crew needed decreases as well as the cost. I chose the values for $r$ based on the rate with optimal crew numbers for minimal cost.
#As the rate increases, the crew decreases and the cost decreases.

# (c)
ans.days=0
ans.cost=0
ans.cost1 <- na.omit(ans.cost)
f = seq(0,50000,10000)
for (i in 1:length(f)){
  ## Change the fine part of the function to the variable f[i] to iterate the changing fines
  r=5/7
  fT = function(x){return(500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*f[i])}
  dfT = function(x){fprime(fT,x)}
  ans.days[i] = bisection(dfT,14,10000)
  ans.cost1[i] = fT(ans.days[i])
}
result = data.frame(fine = f, days = ans.days, cost = ans.cost1)
print(result)

# (d)
# The company thinks the amount of fine is excessive. Assuming the only purpose of the fine is to motivate the clean-up, is the fine excessive?
# I think it is fine, they provide a reasonable amount of time in the first place to get the clean-up done.

### Problem 5 - Fin Whale ###

# (a)

#Variables/Facts:
  
# $R$ = growth rate of the fin whale population (per year)
# $r$ = intrinsic growth rate = $0.08$
# $K$ = maximum sustainable population = $400,000$
# $x$ = current population, $x_0$ = $70,000$
# $H$ = number of whales harvested per year = $0.00001*E*x$
# $E$ = level of fishing effort in boat-days

#Assumptions:
  
# growth rate is constant
# whale harvest rate only depends on $E$ and $x$

## Define the function using boolean statements such as >, <=, == 
# Given: r=0.08, K=400,000, x=70,000
r = 0.08
K = 400000
#x = 70000
# Define the function
fW = function(x){(r*x*(1-(x/K)))} 
# Define the derivative
dfW = function(x){fprime(fW,x)}
# Define the domain to graph on
x = seq(0,900000,100000)
# Plot the function
plot(x,fW(x),type="o")

ans = bisection(dfW,100000,300000)
print(ans)

# The maximum occurs at 200,000.
# The optimal harvest rate is 8,000 whales/year.

# (b)
ans.effort=0
ans.popn=0
#ans.cost1 <- na.omit(ans.cost)
r = seq(0,.15,.01)
for (i in 1:length(r)){
  ## Change r to the variable r[i] to iterate the changing rate
  fW = function(x){return(r[i]*x*(1-(x/K)))}
  dfW = function(x){fprime(fW,x)}
  ans.effort[i] = bisection(dfW,150000,250000)
  ans.popn[i] = fW(ans.effort[i])
}
result = data.frame(rate = r, effort = ans.effort, population = ans.popn)
print(result)

#As the intrinsic growth rate increases by 1% the effort level remains the same and the resulting population increases by 1000 whales.

# (c)
ans.effort=0
ans.popn=0
K = seq(0,800000,100000)
for (i in 1:length(K)){
  ## Change K to the variable [i] to iterate the changing rate
  fW = function(x){return(0.08*x*(1-(x/K[i])))}
  dfW = function(x){fprime(fW,x)}
  ans.effort[i] = bisection(dfW,0,1000000)
  ans.popn[i] = fW(ans.effort[i])
}
result = data.frame(maxsust= K, effort = ans.effort, population = ans.popn)
print(result)

#As the maximum sustainable population increases from 0 to 800,000 the effort level and resulting population remain the same.

### Problem 6 - Whaling ###

# (a)
# Variables/Facts:
  
# $C$ = cost of whaling (dollars/day) = $500$ 
# $F$ = price of fin whale carcass (dollars) = $6,000$
# $R$ = revenue (dollars/year)
# $P$ = profit (dollars/year)

# Assumptions:
  
# cost of whaling and price of fin whale carcass will remain constant
# $P = R - C$
# $R$ = $6000*H$

# Define the function
fG = function(x){-0.0012*(x^2)+490*x-4000000} 
# Define the derivative
dfG = function(x){fprime(fG,x)}
# Define the domain to graph on
x = seq(0,250000,10000)
# Plot the function
plot(x,fG(x),type="o")

ans = bisection(dfG,0,300000)
print(ans)

# The maximum is at 204166.60.

print(fG(ans))
# The maximum value is 46020833.

#Level of Effort: 8,000
#Population: 204,167
#Profit: $46,020,833

# (b)
w=500
ans.prof=0
ans.effort=0
C = seq(2/7,8/7,1/7)
for (i in 1:length(r)){
  ## Change r to the variable r[i] to iterate the changing rate
  fT = function(x){return(-0.0012*(x^2)+(480+.02*w)*x-8000*w)}
  dfT = function(x){fprime(fT,x)}
  ans.prof[i] = bisection(dfT,0,100000)
  ans.effort[i] = fT(ans.crew[i])
}
result = data.frame(cost = C, profit = ans.prof, effort = ans.effort)
print(result)

# (c)
w=500
ans.prof=0
ans.effort=0
F = seq(2/7,8/7,1/7)
for (i in 1:length(r)){
  ## Change F to the variable F[i] to iterate the changing fin whale carcass price
  fT = function(x){return(-0.0012*(x^2)+(480+.02*w)*x-8000*w)}
  dfT = function(x){fprime(fT,x)}
  ans.prof[i] = bisection(dfT,0,100000)
  ans.effort[i] = fT(ans.crew[i])
}
result = data.frame(fin = F, profit = ans.prof, effort = ans.effort)
print(result)

# (d)
# The cost per boat/day is 500 and the cost of one carcass is 6,000. So if they get one whale they have already made 12 times as much money as the boat costs.





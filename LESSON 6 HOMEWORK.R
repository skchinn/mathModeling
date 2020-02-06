### LESSON 6 ###
### Problem 5 - TV ###
# (a)
#s  = number of 19-inch TVs sold (per year)
#t  = number of 21-inch TVs sold (per year)
#p  = selling price of a 19 in set ($)
#q  = selling price of a 21 in set ($)
#C  = cost of manufacturing sets ($/yr)
#R  = revenue from sale of sets ($/yr)
#P  = profit from the sale of sets ($/yr)

#p  - selling price of 19in set is affected by the number of 19in & 21in TV sets sold:  p=339−0.01s−0.003t 
#q  - selling price of 21in set is affected by the number of 19in & 21in TV sets sold:  q=339−0.004s−0.01t 
#R  - revenue is made only from selling these two TV sets:  R=ps+qt 
#C  - there is a fixed cost and enough of other materials not to cause any additional cost for making more sets:  C=400000+195s+225t 
#P=R−C 
#s≥0 
#t≥0

# (a)
library(MASS);library(NlcOptim)
f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]
                 +(399-0.004*x[1]-0.01*x[2])*x[2]
                 -(400000+195*x[1]+225*x[2]))*(-1)-(25*x[1])-(25*x[2])}
x = c(4700,7042)
ans = optim(x,f,method="BFGS")
return(ans)
Z = Outer(f,x)
A = matrix(c(1,0,0,1))
B = matrix(C(0,0))
abline(h=0,col="red",lwd=3)
abline(v=0,col="red",lwd=3)
ans = solnl(x,objfun=f)
print(ans)
# Optimal production for 19-inch TV is (5661) 5660.96 and for 21-inch TV is 7969 (from 7968.66).

s = 5660.96
t = 7968.66
tariff=25*t+25*s
# The tariffs cost the company $340,740.50 per year.

# (b)
# C = 200,000x-550,000x
# x = years
ans = 200000-550000
print(ans)
ans1=ans+tariff
print(ans1)
# Company loses $9259,50.

# (c)
Tar = function(t){
  f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*(-1)-(t*x[1])-(t*x[2])}}
x = c(4700,7042)
s = seq(0,25,5)
ans = array(0,length(s))
for (i in 1:length(g)){
  profit = function(x){
    return(function(t){
      f = function(x){((339-0.01*x[1]-0.003*x[2])*x[1]+
                         (339-0.004*x[1]*x[2])*x[2]-
                         (400000+195*x[1]+225*x[2]))*(-1)-(t*x[1])-(t*x[2])}
    })
  }
}
  
  
# (d)
# The tariff is essential in indirectly controlling where the businesses reside. If the tariff is high the company is more likely to stay in the country. 
# If the tariff is not high enough, then the company can move outside the country without losing too much.
# However, the tariff cost can not be too high to where the company is annoyed at the money they are paying.
  


### Problem 7 - Newspaper ###
library(ma391chinn)
# Variables:
# p = subscription price ($/week) = 1.5
# s = x[1] = subscribers = 80000
# a = x[2] = advertising revenue ($/page) = 250
# n = number of pages (pages/week) = 350
# P = s*p + a*n

#Assumptions
# 10 cents/week increase in p will cause a loss of 5,000 subscribers
# p = 1.6; s = 75000

# increasing a by $100 causes the n to decrease by 50 per week
# a = 350; n = 300; s = 79000

# (a)
P = function(x){1.5*x[1]+350*x[2]}
x = seq(1,50,1)
bisection

### LESSON 2 ###
### Problem 1 - Car ###
# (a) What amount of rebate will maximize profit?
# Step 1: 

# p = profit on sale model ($1500/car)
# r = rebate $/car
# x = rebate scale factor ($100/car)
# y = sale increase based on rebate (0.15 = 15%)
# n = number of cars sold
# P = total profit

library(ma391chinn)

# Step 2: Select the modeling approach
# Single Variable Optimization

# Step 3: Formulate the model
f = function(r){(1500-r)*(1+0.15*r)}
df = function(r){fprime(f,r)}

# Step 4: Solve the model
r = seq(0,1500)
ans = bisection(df,0,1500)
print(ans)
print(f(ans))

# The rebate that maximizes the profit is $746.67.
# The profit with that rebate is $85,126.67.

# (b) Sensitivity Analysis

sales = seq(0.05,0.5,0.01)
ans = 0
ans.profit=0
for (i in 1:length(sales)){
  f = function (r){
    return((1500-100*r)*(1+sales[i]*r))
  }
  df = function(r){fprime(f,r)}
  ans[i] = bisection(df,-1000,2000)    
  ans.profit[i]=f(ans[i])
}
result = data.frame(salesIncrease=sales,optimalRebate=ans,profit=ans.profit)
print(result)
plot(sales,ans,"o",xlab="Sales Increase",ylab="x (# of rebates)")

# (c) 
f = function(r){(1500-r)*(1+0.10*(r/100))}
df = function(r){fprime(f,r)}
r = seq(0,1500)
ans = bisection(df,0,1500)
print(ans)
print(f(ans))

# If rebates only generate 10% increase then the maximum
# rebate should be $250.03 which would make profit $1562.50.

# (d)
# Somewhere between 0.07 and 0.08. Small sale increase and larger rebate.

### Problem 2 - Pig ###

# x = time (days)
#Sensitivity of Feed Cost of Pig
feed = seq(0.3,0.6,0.05)
pr = array(0,length(feed))
ans = array(0,length(feed))
for (i in 1:length(feed)){
  profit = function (x){
    return((0.65-0.01*x)*(200+5*x)-feed[i]*x)
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
  pr[i] = profit(round(ans[i]))
}
print(ans)
print(pr)
plot(feed,ans,"o",xlab="feed cost",ylab="x(Days to Sell)")
title("Sensitivity of Feed Cost of the Pig")

# .60 per day feeding / growth rate increase of 7 lbs per day / assume 132.8 was previous profit
profit = function (x){
  return((0.65-0.01*x)*(200+7*x)-0.6*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o",xlab="x(days)")
abline(h=132.8) #represents the max profits for previous feed
dProfit = function(x){fprime(profit,x)}
ans=bisection(dProfit,0,20)
print(ans)
profit(round(ans))

# Yes, wait around 14 days (from 13.92) to increase profit $143.58.

















### Problem 3 - Pig ###
# (a)
p1 = function(x)0.65-0.01*x
p2 = function(x)0.65-0.01*x+0.00004*x^2
t = seq(0,20)

plot(t,p1(t),"l",lwd=3)
points(t,p2(t),"o",col="red")
legend("top", c("0.65-0.01t", expression(paste("0.65-0.01",t,"+0.00004",t^2))),col=c("black","red"),lty=1)

# (b)
c=0.00004
profit = function (x){((0.65-0.01*x+c*x^2)*(200+5*x)-0.45*x)}
dProfit = function(x){fprime(profit,x)}
t = seq(0,20)
plot(t,profit(t),type="l",col="gray",lwd=3)
bisection(dProfit,0,20)
tab = data.frame(days=t,profit=profit(t))
print(tab)

# The best time to sell the pig is around 11 days (from 10.28).

# (c)
## Sensitivity Analysis of leveling off parameter
lo = seq(0.00001,0.00008,0.00001)
label=0
ans.days = 0
ans.profit=0
t = 0:30
colors = c("red","blue","green","purple","gray","pink","purple","black")
plot(t,col="white",xlab="DaysToSell",ylab="Profit",xlim=c(0,30),ylim=c(130,140)) #set up the plot for the loop
for (i in 1:length(lo)){
  c = lo[i]
  label[i]=sprintf("%.5f",c) #produce string from numbers
  profit = function (x){((0.65-0.01*x+c*x^2)*(200+5*x)-0.45*x)}
  dProfit = function(x){fprime(profit,x)}
  ans.days[i] = bisection(dProfit,0,20)
  ans.profit[i]=profit(ans.days[i])
  points(t,profit(t),col=colors[i],type="l") #individual plots
}
legend("topleft",label,col=colors,lty=1) #legend using the labels created and colors
result = data.frame(levelOff=lo,optimalDays = ans.days,profit=ans.profit)
print(result)

# After conducting sensitivity analysis, we conclude there is not enough of a 
# difference on the profit to consider changing this parameter.
# Simulation of Binomial problem from homework

n = 10000;
m = 500;

x = rep(0,n);
for (i in 1:n) {
  x[i] = sum(runif(m) < .5)
}
hist(x);
y = (x-m/2)/sqrt(m/4)  # mean and variance 0,1.  
y = sort(y)
plot(y,seq(1:n)/n,type='l');
lines(y,pnorm(y));



# the simulation homework problem involving the Poisson process


n = 100000;
lambda = 3;

y = rep(0,n);
for (i in 1:n) {
  z = runif(100);  # 100 Unif(0,1) variables
  x = -log(z)/lambda;  # 100 Exponential(lambda) variables
  y[i] = sum(cumsum(x) < 1);  # this counts the number of Exp(lambda) events that happen in [0,1].  
}
m = max(y);
p = rep(0,m);
phat = rep(0,m);
for (i in 0:m) { 
  phat[i+1] = sum(y == i)/ n; 
  p[i+1] = exp(-lambda) * lambda^i / factorial(i);
}
plot(p);
points(phat,pch='x');

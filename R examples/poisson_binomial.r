# simulation of the pair (x,y) where x ~ binomial(N,p), y = N-x, N ~ Poisson()
# this is an example of a mixture distribution 
# oddly x and y turn out to be independent as one can see from the plot

trials = 1000
N = rpois(trials,lambda=20);
x = rbinom(trials,size=N,prob=.5);
y = N-x;
x = x + rnorm(trials,sd=.1);  # add a little noise for ease of viewing in plot
y = y + rnorm(trials,sd=.1);
plot(x,y,pch='x');


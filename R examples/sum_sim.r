# program to demonstate the CLT = central limit theorem.  CLT says sums of independent rv's are approx normal
# with better approx as we add in more and more rv's to the sum.  Here we check this by looking at the
# distribution of the sum of k Unif(0,1)'s by simulating the variable n times.
# we compare this with the distribution of the corresponding N(k/2,k/12) variable
# (recall that the sum of k Unif(0,1)'s has mean k/2 and variance k/12)

k = 3        # number of Unif(0,1)'s we will add up
n = 100000   # number of times we do this experiment
y = rep(0,n) # hold outcomes of expt here
yy = rep(0,n) # hold normal probabilities here

for (i in 1:n)   y[i] = sum(runif(k));  # simulate n sums of k Unif(0,1)'s
s = sort(y);	      			# sort the outcomes for plotting
plot(s,(1:n)/n,type='l');   		# this is plot of estimate of P(Y <= y) for all observed values of y
yy = pnorm(s,k/2,sqrt(k/12));		# these are the probs of a N(k/2,k/12) <= y for all observed values of y
lines(s,yy,lty=3);			# show both on same plot

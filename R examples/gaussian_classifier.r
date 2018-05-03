# a simple two-class Gaussian problem.  The graph depicts p(c)p(x|c) for the two classes c=1,2.
# The c that maximizes this product is the choice that will minimize p(error|x).


p = c(.3,.7);   # the class probabilitiess
m = c(0,1);  # the class means
s = c(1.5,.5);  # the class std deviations  

x = seq(-7,7,by=.01);   # the range we will plot over
N = length(x);
pr = matrix(0,2,N);	# this holds the values of p(c)p(x|c)
for (i in 1:2) {
    pr[i,] = p[i]*dnorm(x,m[i],s[i]);
}
mx = apply(pr,2,max);	# for each x take the maximal value of p(c)p(x|c)
plot(x,mx,pch="");
for (i in 1:2) {
    lines(x,pr[i,],lty=i);
}

# In this example (from HW3) we use the fact that the normal is the conjugate 
# prior for the normal when estimating the mean with known variance.  We 
# begin with an uninformative prior distribution by choosing the variance 
# of the normal to be very large and choosing the initial mean aribitrarily 
# to be 0.  After each observation from a N(10,1) distribution (we know the 
# variance but don't know the mean) we recompute the conditional mean and 
# variance for normal mean, and plot this distribution.  As we accumulate 
# data the variance of our posterior distribution get smaller and smaller, 
# reflecting the fact that our knowledge about the true mean is more and 
# more certain. Also the conditional mean of our posterior distribution 
# converges to the true mean (10).  


nu = 0;
rho2 = 100000000;
sigma2 = 1


for (i in 1:100) {
  t = seq(-20,20,by=.01);
  plot(t,dnorm(t,mean=nu,sd=sqrt(rho2)),type='l');
  readline();
  x = rnorm(1,mean=10,sd=1);
  nu = (nu/rho2 + x/sigma2)/(1/rho2 + 1/sigma2);
  rho2 = 1/(1/rho2 + 1/sigma2);
}



# a) above
# b) the conditional density becomes more and more peaked around 10 (the true mean)
# c) the expectation converges to 10
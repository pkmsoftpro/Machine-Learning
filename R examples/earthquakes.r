# The number of mild earthquakes in a paticular place and month is modeled as
# a Poisson variable.  
# We do not know the important Possion parameter which is the average number
# per month.  Our prior opinion on the number of earthquakes is modeled as 
# Gamma(theta,k) (because this makes calculation easy).  
# Since we don't know theta and k we choose values that lead to a somewhat
# unopinionated distribution.  Though we don't know it,
# the real rate of earthquakes in our place of interest is lambda = .3 and will use this
# in our simulation.  After every month we update our Gamma parameters which
# represent our current belief about the earthquake rate.  

months = 1000;
theta_hat = rep(0,months);  # estimate of theta in nth month
k_hat = rep(0,months);      # estimate of k in nth month
count = rep(0,months);  # counts of earthquakes for the different months
range = seq(0,2,by=.01);  # range where we will plot Gamma density
theta_hat[1] = 30;     # initial prior on the Poisson parameter is relatively unopinionated:
k_hat[1] = 1
plot(range,dgamma(range,shape=k_hat[1],scale=theta_hat[1]),type='l');
lambda_true = .3;      # this is true rate which we don't know


for (i in 2:months) {
  x = rpois(lambda=lambda_true,n=1);	# observe the number of earthquakes in month i
  k_hat[i] = k_hat[i-1] + x;		# update Gamma parms according to update formula
  theta_hat[i] = 1/((1/theta_hat[i-1]) + 1);
  plot(range,dgamma(range,shape=k_hat[i],scale=theta_hat[i]),type='l');  # plot current "knowledge" on lambda
  print(paste("earthquakes in month ", i, "is: ",x, "posterior mean is ",k_hat[i]*theta_hat[i]));  
  # recall that posterior mean gives the estimate the minimizes E(sq error).  The mean of 
  # a Gamma(theta,k) is theta*k.  
  readline();
}

# NB.  As we observe more months our opinion of lambda gets more and more concentrated.
# said another way the posterior variance decreases to 0.  

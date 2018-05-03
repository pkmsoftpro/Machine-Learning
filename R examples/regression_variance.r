# program to demonstrate estimation of simga^2 in 
# linear regression model:  y = Xw + e  where
# e ~ N(0,sigma^2 I)

n = 100;
k = 10;		# try different values here
X = matrix(rnorm(n*k),ncol=k,nrow=n);  # predictors chosen randomly
w = seq(1,2,length=k);  # could be anything
sigma2=20.;	     # the true variance which we also don't know
y = X %*% w + rnorm(n,0,sqrt(sigma2));  # y variables created according to linear model
what = solve(t(X)%*% X, t(X) %*% y);  # solve normal eqn's
yhat = X %*% what;   # our predicted y's based on the x's.  
e = y - yhat;	     # the prediction errors
sigma2hat = sum(e*e)/(n-k);	      # estimate of sigma^2 (note demominator of n-k (not n))
print(sigma2hat);		      # repeated trials suggest sigma2hat is UE for sigma
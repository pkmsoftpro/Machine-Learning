# Example of simple linear regression.  Our target or response vector y 
# is generated by a random walk. Each column of the data matrix, X, is 
# a predictor we will use to try to approximate y.  The columns of X
# are taken to be 1, x, x^2, x^3 ... where x is a vector evenly spaced 
# numbers from -1 to 1 (of the same length as y). The predictor variables 
# don't seem to have anything to do with the target variable, and this is 
# intentional. Still linear regression manages to get a good approximation of 
# y as a linear function of the predictors.  

n = 100	       		           # number of observations
y = cumsum(runif(n,max=1,min=-1))  # y is a random walk,  this generates complicated looking functions
plot(y,type='l');
k = 10;				   # number of predictor variables, a.k.a. "features"
X = matrix(0,n,k);
t = seq(-1,1,length=n);  #  
for (j in 1:k) X[,j] = t^(j-1);  # jth feature is x raised to (j-1) power
#for (j in 1:k) lines(X[,j]);
#stopp;
what = solve(t(X) %*% X , t(X) %*% y);   # solve solves Ax = b (this is w-hat and not "what"!!!)
yhat = X %*% what;
lines(yhat);

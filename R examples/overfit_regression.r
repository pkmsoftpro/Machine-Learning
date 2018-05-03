# program to demonstrate overfitting with simple linear regression.  


# make data by adding noise around a linear model (y = a + bx + e)
n = 100;
y = rep(0,n);
x = seq(0,1,length=n);
for (i in 1:n) y[i] = 2*x[i] + 5 + rnorm(1,sd=.1);
plot(x,y);


# fit the model using knowledge of the underlying equation and plot fit
k = 2;
X = matrix(0,nrow=n,ncol=k);
X[,1] = rep(1,n);
X[,2] = x;
what = solve(t(X) %*% X) %*% t(X) %*% y;
yhat = X %*% what;
lines(x,yhat);
print(paste("2-feature SSE = ",sum((yhat-y)^2)))


# now include additional garbage (noise) features and observe fit.  increase k until k = n;
k = 100;  
X = matrix(0,nrow=n,ncol=k);
X[,1] = rep(1,n);
X[,2] = x;
for (j in 3:k) X[,j] = rnorm(n);
what = solve(t(X) %*% X) %*% t(X) %*% y;
yhat = X %*% what;
lines(x,yhat);  # dots = orig data, lines = fitted model --- fit is great!!!
print(paste("k-feature SSE = ",sum((yhat-y)^2)))



# now use identical tranied model (what) but with new x's and y's generated just like before
X[,1] = rep(1,n);
X[,2] = x;
for (j in 3:k) X[,j] = rnorm(n);
for (i in 1:n) y[i] = 2*x[i] + 5+ rnorm(1,sd=.1);
yhat = X %*% what;
plot(x,y);
lines(x,yhat)   # fit is very bad!!!
print(paste("new data SSE = ",sum((yhat-y)^2)))



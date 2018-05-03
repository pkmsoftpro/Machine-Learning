

n = 10000	     	      	       # number of samples
k = 5			       	       # dimension of data we create
A = matrix(rnorm(k*k),nrow=k,ncol=k);  # transformation matrix
X = matrix(0,n,k);		       # will hold data	
for (i in 1:n) {			
  z = rnorm(k);				# as before
  x =  A * z;				
  X[i,] = t(x);	 			
}
#plot(X)
pairs(X)				# show pairs plot
#print(cov(y))				# print empirical covariance matrix
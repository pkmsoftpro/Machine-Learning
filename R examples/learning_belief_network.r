# This examples creates a joint distribution with conditional independencies.  We would like to to uncover the conditional independences from looking at the data.  
# The variables in this construction are normal.  This analysis uses a property of the "concentration" matrix, which is the inverse of the covariance matrix.
# (recall this matrix appears in the pdf of the joint normal)
# For a joint normal vector  the (i,j) element of the concentration matrix is 0 if and only if x_i and x_j are conditionally independent given
# all other variables.  


n = 100000;         # number of data points, variables are w,x,y,z
w = rnorm(n);		
x = rnorm(n,mean=w);
y = rnorm(n,mean=3*w+10);
z = rnorm(n,mean=x-y);

# Considering the variables in order: w,x,y,z, we see from the construction that:
# x,y conditionally indpendent given w
# z,w conditionally independent given x,y



X = matrix(c(w,x,y,z),ncol=4,nrow=n);	# represent data as data matrix, 1st variable (column) is w, 2nd variables is x ...
 pairs(X);  # notice that no pair of variables are independent				
S = cov(X);  # the empirical covariance of the data.


# First consider the marginal distribution of w,x,y and observe the concentration matrix

print(solve(S[1:3,1:3]))  # solve is inverse.  # note 2,3 element is nearly 0 hence it appears that x_2, x_3 CI given x_1
			  # that is x,y CI given w


# Now consider the concentration matrix for all 4 variables together

print(solve(S))  # note that 1,4 element is nearly 0.  thus x_1, x_4 CI given x_2,x_3
		# that is w,z CI given x,y.
# thus the joint dist factors as p(w,x,y,z) = p(w)p(x|w)p(y|w,x)p(z|w,x,y)
#  = p(w)p(x|w)p(y|w)p(z|x,y)

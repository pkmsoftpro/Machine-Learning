# demonstration of computation of finding the "maximum-margin" 
# separating hyperplane of # linearly separable two-class dataset

library("quadprog");  # we do this by quadratic programming and need library

w = rnorm(2);  # choose a random weight vector (unknown to us)
b = rnorm(1);  # choose the b offset (also unknown)
p = .05;
C = .1;
N = 100;       # number of observations
X = matrix(0,nrow = N, ncol = 2);  #  the data matrix of predictors
t = rep(0,N);  # the vector of {-1,1} elements giving classes 
for (i in 1:N) {  # generate the data
  X[i,]  = rnorm(2);  # choose each predictor vector randomly
  t[i] = sign(t(w) %*% X[i,] + b)  # choose data to be linearly separable
  if (runif(1) < p) t[i] = -t[i];
  # note that our choice of the class is forced here if we want
  # to be sure the classes are linearly separable.  
  # we should expect that there are other separating hyperplanes than
  # the one defined by our choice of w and b
}
plot(X,pch=(t+2),col=(t+3),cex=3)   # observe dat
#abline(-b/w[2],-w[1]/w[2]);  # draw the separating line

# now we want to phrase the problem as quadratic programming.  
# the generic formulation is: 
#  minimize x^t D x + d^t x s.t. A x >= c
# where 
#  x is a k-vector 
#  D is kxk matrix
#  d is k-vector
#  A is Nxk matrix
#  c is  N-vector
#
# We showed that "our" QP problem can be written out in this form where
#
# D is 0-1 matrix with only the first two diagonal elements 1
# d is 0
# The first two columns of A are the X matrix with sign flips according to t
# and the last column is t
# c is a vector of 1's

# construct the corresponding elements: D,d, A, c

D = diag(3+N);
D[3,3] = .00000001
for (n in 1:N) D[3+n,3+n] = 1;  # first two components are the w elements and last one b
# minor kludge here,  need a positive definite matrix so add a small
# value for the last diagonal element
d = rep(0,N+3);
for (n in 1:N) d[3+n] = C;
A1 = cbind(X*t,t,diag(N));
A2 = cbind(matrix(0,nrow=N,ncol=3),diag(N));
A = rbind(A1,A2);
c = c(rep(1,N),rep(0,n));
result = solve.QP(D,d,t(A),c);  # painless QP result from package!!

what = result$solution[1:2];  # margin-maximizing w
bhat = result$solution[3];    # margin-maximizing b
abline(-bhat/what[2],-what[1]/what[2]);  # draw the separating line
# abline gives line in mx+b form so need to transform our line represention
# which is w^t x b = w_1 x + w_2 y + b = 0 to 
# y = -w_1/w_2 x - b/w_2
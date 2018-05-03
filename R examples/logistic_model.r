# simple example of logistic regression with data generated
# from the logistic model

wtrue = c(3,-2)  # the true weight vector (unknown to us)
n = 500      # number of observations
k = 2        # dimension of observations
X = matrix(rnorm(n*k),ncol=k);  # matrix of observations
                                # according to logistic model doesn't matter
				# what dist of X is.  
				# customary to put data vectors in matrix where
				# each row is an observation
				# each column a variable
p = 1/(1 + exp(-X %*% wtrue))   # p[i] = p(class = 1 | x[i]) 
    	   	      		#      = sigma((Xw)[i])
    	   	      		# %*% is matrix mult
c = as.integer(runif(n) < p)    # generate classes according to p
    			        # c[i] = P(runif < p[i]) = p[i]
plot(X,pch=c,col=c+3);          # visualize


w = c(0,0);   			# initial weight vector

for (iter in 1:100) {  # iterations of gradient descent on log likelihood
  p = 1/(1+exp(-X %*% w))   # prob of c=1 under current weight
  ll = sum(log(p[c == 1])) + sum(log(1-p[c == 0]))  # the log likelihood of data
  print(ll);  # note that ll is strictly increasing


  grad = rep(0,k);  # compute the gradient
  for (i in 1:n) {
    grad = grad + (c[i] - p[i])*X[i,];
  }
  w = w + .01*grad;  # small step in direction of greatest increase of ll
}

chat = (p > .5);		 # our estimate of the class in the end
plot(X,col=((c != chat)+3))	 # color misclassified points differently
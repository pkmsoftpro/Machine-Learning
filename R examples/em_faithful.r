# program to demonstrate em algorithm for gaussian mixture model on old faithful data

data(faithful);
library(mvtnorm);	# using multivariate normal package (need to install this but easy .... )
N = dim(faithful)[1];	# number of sample points
X = faithful[,1:2];	# the data matrix
pi1 = pi2 = .5;		# these are our initial class probability
m1 = c(2,90);  		# initial means, chosen to be bad
m2 = c(4,50);  
Sigma1 = Sigma2 = diag(c(var(X[,1]),var(X[,2])));   
       	 # initial covariance matrices computed from entire data

for (iter in 1:30) {  # iterations of em algorithm
  prob1 = pi1*dmvnorm(X,m1,Sigma1);   # probabilities of sample points under model 1
  prob2 = pi2*dmvnorm(X,m2,Sigma2);   # same for model 2
  xmarg = sum(prob1,prob2);	      # marginal probs of x
  ll = sum(log(xmarg));	 	      # the log likelihood of the data under 
       				      #  mixture model --- this is guranteed to increase
  print(paste("loglikelihood = ", ll),quote=F);	# print it out
  resp1 = prob1 / (prob1 + prob2);    # the responsibilities under model 1
  resp2 = prob2 / (prob1 + prob2);    # same for model 2
  plot(X[,1],X[,2],col = rgb(resp1,resp2,0),pch=16,cex=2);   
# plot points using a color to represent soft assignment
# red is model 1, green is model 2, between red and green means partial 
# assignment

  points(t(m1),pch='X',col=2,cex=2);   # show the current class means
  points(t(m2),pch='X',col=3,cex=2);
  readline();				# wait for use input
  N1 = sum(resp1); N2 = sum(resp2);	# N1,N2 are the total responsibilities claimed by 
       		      			# class 1 and class 2
  pi1 = N1/N; pi2 = N2/N;		# reestimate of class probabilities
  m1 = colSums(resp1*X)/N1;  		# reestimate of class means
  m2 = colSums(resp2*X)/N2;	
  acc1 = acc2 = matrix(0,nrow=2,ncol=2);
  Y = as.matrix(X);			# reestimate of class covariances
  for (n in 1:N) {
    acc1 = acc1 + resp1[n] * ((Y[n,] - m1)  %*% t(Y[n,]-m1));   
    acc2 = acc2 + resp2[n] * ((Y[n,] - m2)  %*% t(Y[n,]-m2));   
  }
  Sigma1 = acc1/N1; Sigma2 = acc2/N2;
}

# demonstrate the EM algorithm for estimating a Gaussian Mixture Model (GMM)

n = 50			# samples for each class
K = 3			# number of clusters
D = 2;			# dimension of points
iterations = 100;	# iterations of em algorithm

library(mvtnorm);	# need to install this package to make this example work 
N = K*n;		# total number of pts. 
cent = array(10*rnorm(K*D),c(K,D));  # cent[k,] is center of kth cluster (chosen randomly)
T = array(rnorm(K*D*D),c(K,D,D));    # T[k,,] is transformation used in generating kth cluster
X = matrix(0,nrow=D,ncol=0);	     # data matrix, as usual
for (k in 1:K) {
    m = cent[k,];
    t = T[k,,];
    for (i in 1:n) X = cbind(X,t %*% rnorm(D)+m);  # each point adds a new column
}
X = t(X);  # take transpose so the rows are the observations (as usual)
plot(X);		       # show the results
Sigma = array(0,c(K,D,D));     # Sigma[k,,] will be kth covariance
for (k in 1:K) Sigma[k,,] = 10*diag(D);  # initialze to be large = high-variance = unopinionated
mu = array(rnorm(K*D),c(K,D));	# mu[,] is kth mean initialzed randomly
pi = rep(1/K,K);		# initialization of the class probabilities (uniform)
r = matrix(0,N,K);		# r[i,k] is the prob of kth class given ith sample
    				#  (the responsibilities)
M = rep(0,K);			# M[k] is the number of samples attributed to class k
cluster = rep(0,N);		# cluster[i] is the cluster (in 1 .. K) estimated for ith point
	  			# not using cluster in the gmm code
for (j in  1:iterations) {
    for (i in 1:N) {
    	for (k in 1:K) r[i,k] = pi[k]*dmvnorm(X[i,],mu[k,],Sigma[k,,])  # dmvnorm is normal density
	r[i,] = r[i,] / sum(r[i,]);	# r[i,k] = P(C = k | X[i,])
	cluster[i] = which.max(r[i,]);  # assign each point to most likely cluster
    }
    for (k in 1:K) {
    	M[k] = sum(r[,k]);	# estimated count of examples from kth cluster
	pi[k] = M[k]/N;		# estimated proportion belonging to kth class
	mu[k,] = 0;		
	Sigma[k,,]=0;
	for (i in 1:N)  mu[k,] = mu[k,] + (r[i,k]/M[k])*X[i,];  # reestimate mu[k,] and Sigma[k,,]
        for (i in 1:N) Sigma[k,,] = Sigma[k,,] + (r[i,k]/M[k])*(X[i,] - mu[k,]) %*% t(X[i,]-mu[k,]);  
    }
#    plot(X[,1],X[,2],col=cluster);  # do this for more than 3 clusters
#     points(proto,pch='x',col=1:K,cex=3);   	 # plot cluster centers as well
    plot(X[,1],X[,2],col = rgb(r[,1],r[,2],r[,3]));   # when we have 3 clusters do this
    points(mu,pch='x',col=c(rgb(1,0,0),rgb(0,1,0),rgb(0,0,1)))
      # r[1,] r[,2], r[,3] are the probs of classes 1,2,3.  Make colors that reflect the likelihood of the
      # classes.  
      # if probs are (1,0,0) use red
      # if probs are (0,1,0) use green
      # if probs are (0,0,1) use blue
      # ow mix these colors using the 3 probabilities 

    readline();   # wait for use input
}





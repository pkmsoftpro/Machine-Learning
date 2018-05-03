# demonstrate the k-means algorithm on "old faithful" dataset.  
# old faithful data has time between geyser eruptions and length of eruption for 272 observations.
# want to cluster these into 2 clusters.


data(faithful);
plot(faithful);
#stopp;
X = faithful[,1:2];	# the data matrix
n = dim(X)[1];		# number of observations
m = matrix(0,2,2);
for (i in 1:2) {
    X[,i] = X[,i] - mean(X[,i]);	# standardize each component to be 0 mean and variance 1.  Could skip this.
    X[,i] = X[,i] / sqrt(var(X[,i]));
}
#m[1,] = c(-1,1);  # the rows of m are the class means.  these are chosen to be deliberately bad.
#m[2,] = c(1,-1);  

m[1,] = c(0,-.5);   # use different starting place
m[2,] = c(.1,.8);


for (iter in 1:5) {  # iterations of k-means
  d = m[1,]-m[2,];   # next 4 lines compute the line ax = b that separates the classes.  
  a = -d[1]/d[2];
  c = (sum(m[1,]*d) + sum(m[2,]*d))/2
  b = c/d[2];

  z = (a*X[,1] + b > X[,2]);    # boolean vector of class assignments --- doesn't generalize to 3 or more classes
                                # with 3 or more classes would have to find closest m_k for each point (easy)
  plot(X[,1],X[,2],asp=1,col=z+2,cex=2);   # plot X with different colors for each (current) cluster
  points(m,pch='X',col=c(2,3),cex=2);      # show the class centers, (rows of m) 
  abline(b,a);			     # draw separating line
  readline();			     # wait for user to type a key so we can look and see what happened
  m[1,] = colMeans(X[!z,])	     # reestimate the new centers as the sample means of the current clusters
  m[2,] = colMeans(X[z,])
  points(m,pch='X',col=c(2,3),cex=2);	     # show the new cluster centers
  readline();			     # wait for more input
}






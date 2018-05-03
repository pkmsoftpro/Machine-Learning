# the famous iris data from Fisher are classified with a logistic regression.  
# We look at the problem as a two-class problem trying to 
# distinguish the setosa iris from the others.  
# Simple gradient descent with a small step size is used.  

data(iris);  # R just knows about this data set
d = 4;
class= as.integer(iris[,5] == "versicolor");  # class is 1/0, 1 for setosa 0 for others
# also can try "virginica" and "versicolor"

n = dim(iris)[1];  # the number of observations
X = as.matrix(iris[,1:4]);  # each row of X is an observation

w = rep(0,d);   # the weight function of the features


pairs(X,pch=class,col=class+3);  # visualize the data

for (iter in 1:200) {  # iterations of gradient descent on log likelihood
  ll = 0;  # the log likelihood
  for (i in 1:n) {
      if (class[i] == 1) { ll = ll + log(1/(1+exp(-sum(w*X[i,])))); }  # log(p(c=1|x))
      else { ll = ll +  log(1-1/(1+exp(-sum(w*X[i,])))); }   # log(p(c=0|x))
  }   
  print(ll);  # note that ll is strictly increasing

  grad = rep(0,d);  # compute the gradient
  for (i in 1:n) {
    grad = grad + (class[i] - (1/(1+exp(-sum(w*X[i,])))))*X[i,];
  }
  w = w + .001*grad;  # small step in direction of greatest increase of ll
}

classhat =  (1/(1+exp(-(X %*% w))) > .5)  # the classification given by logistic regression
print(paste("num errors =",sum(classhat != class)))    # well, it wasn't very hard ...

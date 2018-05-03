# A multivariate Gaussian classifier applied to the Fisher iris data.   The prior probabilities are taken to be equal since the three classes
# appear in equal measure.  Really this is probabably because the data were collected to represent the three classes equally, so one can
# think of this as maximum likelihood classification rather than an optimal Bayes classifier.  

# In this problem we get about the same results when we test on the training data as when we test on data separate from the training.  
# In general you should expect your generalization error (the error on data other than the training) to be worse than what is
# exhibited on the training set since the parameters were tuned for your training set.  But the degree to which it is worse
# will depend on the flexibility of your classifier.  A classifier that learns only a few parameters, such as the Gaussian classifiers
# here, shouldn't be much worse in generalization since they don't have much ability to overfit the training data.

data(iris);
f = 4;

virg = iris[iris[,5] == "virginica",1:f]
mv = as.matrix(colMeans(virg));
Sv = as.matrix(cov(virg))
Cv = solve(Sv)
dv = det(Sv)

vers = iris[iris[,5] == "versicolor",1:f]
mr = as.matrix(colMeans(vers));
Sr = as.matrix(cov(vers))
Cr = solve(Sv)
dr = det(Sv)

seto = iris[iris[,5] == "setosa",1:f]
ms = as.matrix(colMeans(seto));
Ss = as.matrix(cov(seto))
Cs = solve(Sv)
ds = det(Sv)

n = dim(iris)[1];

plot(iris[,1],iris[,2],pch=' ');   # blank plot to set axes
points(virg[,1],virg[,2],pch='v')
points(seto[,1],seto[,2],pch='s')
points(vers[,1],vers[,2],pch='r')

l = rep(0,3);
for (i in 1:n) {
    x = t(as.matrix(iris[i,1:f]));
    l[1] = .5*log(ds) - .5*t(x-ms) %*% Cs %*% (x-ms)
    l[2] = .5*log(dr) - .5*t(x-mr) %*% Cr %*% (x-mr)
    l[3] = .5*log(dv) - .5* t(x-mv) %*% Cv %*% (x-mv)
   print(which.max(l))
}



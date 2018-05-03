


k = 13;
n = 1000;
p = matrix(runif(2*k),nrow=k,ncol=2);
p[,1] = seq(.2,.8,length=k);
p[,2] = seq(.8,.2,length=k);
X = matrix(0,n,k);
c = 1+rbinom(n,1,.5);
for (i in 1:n) {
    u = runif(1);
    for (j in 1:k) {
#        X[i,j] = rbinom(1,1,p[j,c[i]]);
        X[i,j] = if (u < p[j,c[i]])  1 else 0;
    }
}

train = (1:n) <= (n/2);

TX = X[train,];
tc = c[train];
phat = matrix(0,nrow=k,ncol=2);
for (z in 1:2) {
    for (j in 1:k) {
    	phat[j,z] = sum(TX[tc==z,j])/sum(tc==z);
    }
}

TX = X[!train,];
tc = c[!train];
like = matrix(0,(n/2),2);
for (i in 1:(n/2)) {
    for (z in 1:2) {
    	like[i,z] = prod(phat[,z]^TX[i,]*(1-phat[,z])^(1-TX[i,]))
    }    
}
est = apply(like,1,which.max)
perror = sum(est != tc)/(n/2);



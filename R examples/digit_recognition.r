# digit recognition by classification tree.  example demonstrates how to
# choose "best" tree using cross validation.  More explicitly, we
# form R_a(T) = R(T) + a|T| 
# and find the a the best a by cross-validation.  We can construct tree easily
# once we know a.  

library(rpart)
set.seed(1234);  # want random experiments to be exaclty repeatable
n = 200;	 # number examples
y = rep(0:9, length = 200);	# the true classes
temp = c(1,1,1,0,1,1,1,		# pattern for 0
         0,0,1,0,0,1,0,		# pattern for 1	
         1,0,1,1,1,0,1,		# pattern for 2
	 1,0,1,1,0,1,1,		# ...
	 0,1,1,1,0,1,1,
	 1,1,0,1,0,1,1,
	 0,1,0,1,1,1,1,
	 1,0,1,0,0,1,0,
	 1,1,1,1,1,1,1,	
	 1,1,1,1,0,1,0);
lights = matrix(temp,10,7,byrow=T);	# true light[i,] is bit pattern for i-1
temp1 = matrix(rbinom(n*7,1, .9), n, 7)
temp1 = ifelse(lights[y+1,] == 1, temp1, 1-temp1); #  digits with flipped bits
temp2 = matrix(rbinom(n*17,1,.5), n, 17)  # random lights (noisy worthless features)
x = cbind(temp1,temp2);  # variables 8 ... 24 are useless noise
fit = rpart(y ~ x, method = "class", control = rpart.control(xval=10,minbucket=2,cp=0));
printcp(fit)  	# note the included noise variables
fit9 = prune(fit,cp=.02);  # pruned tree.  anything in the range .01666 to .05 gives same result
#printcp(fit9);
par(mar = rep(.1,4));
plot(fit9,branch = .3, compress = T);
text(fit9);  

# example to demonstrate overfitting of a classification tree.  
# first create a random test set with random labels and train a decision
# tree from the data.  Note there is no way to make a good classifier
# for this situation

library(rpart)
n = 1000						# number of samples
d = 5					 	# dimension of samples
X = matrix(rnorm(n*d),nrow=n);			# predictor vectors are random #'s
df = data.frame(X)				# R needs data frames for some operations
ctrain = sample(1:2,n,replace=T)		# the classes for each vector (random)
#pairs(X,pch=ctrain,cex=2)
fit = rpart(ctrain ~ X,method="class",minbucket=1,cp=0)	# cp = complexity parameter
# fit a decision tree with min terminal size 1 and complexity penalty 0
plot(fit)	      	       		       # look at complex tree we built
pred = predict(fit,df,type="vector")		# test the classifier on the training data
trainerrors = sum(pred != ctrain)		# almost perfect!!!  (so what)
print(paste("number of errors when testing on training data:",trainerrors));
# now create test data that is statistically identical to original data

Xtest = matrix(rnorm(n*d),nrow=n);
ctest = sample(1:2,n,replace=T)
df = data.frame(Xtest)
pred = predict(fit,df,type="vector")		# get predicted results from model learned above
testerrors = sum(pred != ctest)			# about 1/2 right! (as you should expect)
print(paste("number of errors when testing on statistically equivalent data:",testerrors));


# moral:  there was no possible way to get a good predictive model, but the classification tree still
# finds a way to fit the data nearly perfectly.  But this doesn't really accomplish anything
# useful as seen by poor generalization.  This phenomenom is called "overfitting"

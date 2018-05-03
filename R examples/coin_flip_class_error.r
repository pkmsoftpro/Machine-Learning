# Simple Bayes Classifier
# Have three coins:
# coin 1: P(H) = .45 
# coin 2: P(H) = .5 
# coin 3: P(H) = .55 
# Coins are chosen with equal prob
# Observe N flips
# Observe the behavior of the Bayes' classifier over repeated trials

N = 10000	  # number of flips
trials = 10000  # number of repeated experiments
phead = c(.45,.5,.55);

c = sample(1:3,replace=T,trials);  # the true coin (1,2, or 3)
x = rbinom(trials,N,phead[c])  # random # of H's from Binomial(N,phead[c])
p = matrix(0,3,trials);    # p will hold p(c)p(x|c)
for (i in 1:3) {
    p[i,] = (1/3)*dbinom(x,N,phead[i]);  # dbinom gives binomial probs.
}
chat = apply(p,2,which.max);  # which.max is arg max 
# so this gives the most liklely class a posteriori (Bayes class)



# chat = sample(1:3,replace=T,trials);  # guessing strategy
plot(x,chat) # Show behavior of B.C. 
errorhat = sum(c != chat)/trials;   # phat(error) = errors/trails
print(paste("estimated error is:",errorhat));





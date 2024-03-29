# Demonstration of Simpson's paradox on the famous UCB graduate admissions data.

data("UCBAdmissions"); # import the data
ucb = UCBAdmissions;   # abbreviate "UCBAdmissions" 
print(dimnames(ucb));  # 6 departments acceptance results by gender

print(apply(ucb,c("Gender","Admit"),sum));  # 2-way table of Gender x Admit
mosaicplot(apply(ucb,c("Gender","Admit"),sum));  # mosaic plot clearer ...

# table shows clear gender bias against Female students






















# ... or does it ...?  



mosaicplot(t(ucb[,,"A"]));  # department A seems to *favor* Females
stopp;
mosaicplot(t(ucb[,,"B"]));  # B seems neutral  (admission indep of gender)
mosaicplot(t(ucb[,,"C"]));  # so are others ...
mosaicplot(t(ucb[,,"D"]));  
mosaicplot(t(ucb[,,"E"]));  
mosaicplot(t(ucb[,,"F"]));  

# what is going on !?!?

mosaicplot(apply(ucb,c("Dept","Admit"),sum));  # depts accept rates differ
# to facilitate understanding departments are ordered A ... F from easier to harder



mosaicplot(apply(ucb,c("Dept","Gender"),sum));  # men apply more to easier depts A and B



# morals:  1) causal relataionships can't be determined from "observational" data 
# (data where experimenter doesn't control assignment of categories).  
# 2) marginalizing out over variables can suggest spurious relationships





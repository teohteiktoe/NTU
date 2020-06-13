# ==============================================================================================================
# Purpose:      Demo of CART: Targeted Marketing
# Author:       Neumann Chew
# DOC:          10-10-2017
# Topics:       Decision Trees, Decision Rules, CP, CV Error, Surrogates.
# Data Source:  CreditCard.csv
# Packages:     rpart, rpart.plot
# Notes:        As many of you cannot install Rattle package to use fancyrpartplot(), use prp() in rpart.plot
#               Also showed how to use CART model to make predictions on a set of data.
# ver5 Updates: Added confusion matrix on (cross-validated) trainset & testset
#===============================================================================================================

setwd(getwd())

data1 <- read.csv("CreditCardUpgrade.csv")

# Check that Upgraded and SuppCard are recognized as categorical in R.
summary(data1)

data1$Upgraded <- factor(data1$Upgraded, levels=c(0,1), labels=c("No","Yes"))

data1$SuppCard <- factor(data1$SuppCard, levels=c(0,1), labels=c("No","Yes"))

library(rpart)
library(rpart.plot)			# For Enhanced tree plots via PRP()

set.seed(2004)
options(digits = 3)

# default cp = 0.01. Set cp = 0 to guarantee no pruning in order to complete phrase 1: Grow tree to max.
m1 <- rpart(Upgraded ~ Purchases + SuppCard, data = data1, method = 'class', cp = 0)

# Proper rpart plot from rpart.plot package. Better than plot() in rpart package.
prp(m1)
#type is the label preference, extra is the size
prp(m1, type=2, extra=104, nn.box.col = 'light blue')

# Results of CART as Decision Rules
print(m1)

# Effects of Cost Complexity Pruning at important cp values.
printcp(m1, digits = 3)

# Plot CV error vs cp values
plotcp(m1)
## m1 tree is very small. why?

#Default is 30, only 30 data, therefore only one split

# Default minsplit = 30. Due to small sample size, to reduce this value in order to get bigger tree.
m2 <- rpart(Upgraded ~ Purchases + SuppCard, data = data1, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
print(m2)
#NN=FALSE to maximize the font size
prp(m2, type=2, extra=104,nn=FALSE, nn.box.col = 'light blue')
printcp(m2, digits = 3)
plotcp(m2)
## Why is the values of cp in x-axis different from cp in printcp table?

summary(m2)

# Optimal CP = CP that result in lowest CV error. Too simplistic. ok for now.
# This statistical opinion can be overwritten by expert opinion, if any.
cp.opt <- m2$cptable[which.min(m2$cptable[,"xerror"]),"CP"]


# Prune the max tree m2 using a particular CP value (i.e. a specified penalty cost for model complexity)
m3 <- prune(m2, cp = cp.opt)
print(m3)

# Plot the final tree chosen. fallen.leaves = leave style, 
prp(m3, type=2, extra=104, nn=T, fallen.leaves=T, nn.box.col = 'light blue')

plotcp(m3)
## By default, cases that satisfy the split condition go to the left child node; Other cases go to right child node.

m3$variable.importance
## Purchases is higher in importance than SuppCard in m3. Contributed more towards improving node purity.

predicted1 <- predict(m1, newdata = data1, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(data1$Upgraded, predicted1)


predicted2 <- predict(m2, newdata = data1, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(data1$Upgraded, predicted2)


# A new set of data (i.e. potential client list) can be used to generate m3 Tree Predictions.
# Here, we illustrate CART model predictions on trainset data1.
predicted3 <- predict(m3, newdata = data1, type='class')

# Confusion Matrix can be constructed by applying model prediction on testset.
# Illustrated using trainset data1 as testset is not available.
table(data1$Upgraded, predicted3)


summary(m3)

# ---------------------- END ---------------------------------


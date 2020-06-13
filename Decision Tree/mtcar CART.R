# ==============================================================================================================
# Purpose:      Demo of CART for Continuous Y
# Author:       Neumann Chew
# DOC:          15-10-2017
# Topics:       Decision Trees, Decision Rules, CP, CV Error, Surrogates.
# Data Source:  mtcars
# Packages:     rpart, rpart.plot
#===============================================================================================================

setwd('C:/Users/User/Dropbox/NTU/BC3409 AI in Accounting and Finance/AI Model/Decision Tree')

# Loads a standard dataset mtcars from a base package in R.
# To predict mpg using CART on all Xs.
data(mtcars)

library(rpart)
library(rpart.plot)			# For Enhanced tree plots via PRP()

set.seed(2004)
options(digits = 3)

# default cp = 0.01 (cp=complexity paramete). Set cp = 0 to guarantee no pruning in order to complete phrase 1: Grow tree to max.
cart1 <- rpart(mpg ~ ., data = mtcars, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
print(cart1)
printcp(cart1, digits = 3)
plotcp(cart1)
## Caution: printcp(cart1) shows that if you forgot to change the default CP from 0.01 to 0,
## it would have stopped the tree growing process early. The table shows a lot of further growth at CP < 0.01.



# Optimal CP = CP that result in lowest CV error. Too simplistic. ok for now.
# This statistical opinion can be overwritten by expert opinion, if any.
cp.opt <- cart1$cptable[which.min(cart1$cptable[,"xerror"]),"CP"]

# Prune the max tree m2 using a particular CP value (i.e. a specified penalty cost for model complexity)
cart2 <- prune(cart1, cp = cp.opt)
print(cart2)
printcp(cart2, digits = 3)
## --- Trainset Error & CV Error --------------------------
## Root node error: 1126/32 = 35.2
## var(mtcars$mpg)*(31/32) = 35.2 too, proving that Root Node Error = MSE.
## cart2 trainset MSE = 0.00873 * 35.2 = 0.307 units^2
## cart2 trainset RMSE = sqrt(0.307) = 0.554 units
## cart2 testset MSE = CV error = 0.244 * 35.2 = 8.59 units^2
## cart2 testset RMSE = sqrt(8.8) = 2.93 units.

# Plot the final tree chosen.
prp(cart2, type=2, nn=T, fallen.leaves=T, branch.lty=3, nn.box.col = 'light blue', min.auto.cex = 0.7, nn.cex = 0.6, split.cex = 1.1, shadow.col="grey")
## The number inside each node represent the predicted mean value of Y.


cart2$variable.importance
## Weight has the highest importance, disp is second impt.


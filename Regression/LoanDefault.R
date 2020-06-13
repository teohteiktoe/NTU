# ========================================================================================================
# Purpose:      Demo of Binary Logistic Regression model with multiple X
# Author:       Neumann Chew
# DOC:          25-09-2017
# Topics:       Logistic Regression; Odds Ratios; OR Confidence Intervals.
# Data Source:  default.csv as in ISLR Rpackage
# Packages:     data.table, caTools
#=========================================================================================================
library(data.table)
library(caTools)

setwd(getwd())

default.dt <- fread("default.csv")
default.dt$default <- factor(default.dt$default)
default.dt$student <- factor(default.dt$student)

summary(default.dt)

levels(default.dt$default) # Baseline Y = No
m1 <- glm(default ~ . , family = binomial, data = default.dt)

summary(m1)
## Income is not sig in the presence of balance and student
## To remove Income

m2 <- glm(default ~ . -income, family = binomial, data = default.dt)
summary(m2)

# Output the probability from the logistic function for all cases in the data.
prob <- predict(m2, type = 'response')

# If Threshold = 0.5 ---------------------------------------------
threshold1 <- 0.5
       
default.hat.1 <- ifelse(prob > threshold1, "Yes", "No")
                 
table1 <- table(default.dt$default, default.hat.1)
table1
prop.table(table1)


# Train-Test split ---------------------------------------------------
set.seed(2014)
train <- sample.split(Y = default.dt$default, SplitRatio = 0.7)
trainset <- subset(default.dt, train == T)
testset <- subset(default.dt, train == F)

m3 <- glm(default ~ . , family = binomial, data = trainset)
summary(m3)
## P-value for Student is much higher, indicating less confident of result, than all data case.

m4 <- glm(default ~ . -income, family = binomial, data = trainset)
summary(m4)

OR <- exp(coef(m4))
OR

OR.CI <- exp(confint(m4))
OR.CI

# Confusion Matrix on Trainset
prob.train <- predict(m4, type = 'response')
predict.default.train <- ifelse(prob.train > threshold1, "Yes", "No")
table3 <- table(trainset$default, predict.default.train)
table3
prop.table(table3)
# Overall Accuracy
mean(predict.default.train == trainset$default)

# Confusion Matrix on Testset
prob.test <- predict(m4, newdata = testset, type = 'response')
predict.default.test <- ifelse(prob.test > threshold1, "Yes", "No")
table4 <- table(testset$default, predict.default.test)
table4
prop.table(table4)
# Overall Accuracy
mean(predict.default.test == testset$default)
# ---------------------------------------------------------------------------------



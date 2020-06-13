library("Hmisc")
library(rpart)
library(data.table)

# Step 1 Import Data

setwd("C:/Users/User/Dropbox/NTU/BC3409 AI in Accounting and Finance/AI Model/Decision Tree")

ibm_data.dt <- fread("Lawsuit.csv", nrows=1000)

# RQ4: Which origin, dest and carrier has the highest number of major delays?
#head(flights.dt[major.delay==1, .N, by = .(origin, dest, carrier)][order(-N)], 1)
ibm_data.dt[Gender==1, .N, by = .(Dept, Rank)][order(-N)]



HR_data <- read.csv("Lawsuit.csv", nrows=261)
#HR_data <- read.csv("IBM HR Data Attrition.csv")
dim(HR_data)
head(HR_data)

#split train and test
smp_size <- floor(0.75 * nrow(HR_data))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(HR_data)), size = smp_size)
train <- HR_data[train_ind, ]
test <- HR_data[-train_ind, ]

summary(HR_data) #check NA

boxplot(HR_data$Gender) #check Outlier

#continuos - Look ar the shape of data
plot(density(HR_data$Rank), xlab="Rank", main = "Distribution of Rank")
hist(HR_data$Rank, ylim=c(0,500), xlab="Rank", main = "Distribution of Rank", labels=T, col ="light blue")


#------------------------------------------
#Step 2 : Factor Analysis or Feature Selection

#2A Continuous: https://stackoverflow.com/questions/5446426/calculate-correlation-for-more-than-two-variables
r=cor(HR_data[,unlist(lapply(HR_data, is.numeric))], use="complete.obs")
#cor(HR_data$Age, HR_data$Education, use="complete.obs")
#cor(HR_data$Age, HR_data$YearsAtCompany, use="complete.obs")
#cor(HR_data, use="complete.obs")
r
write.csv(r, file = "Correlation.csv", row.names=TRUE)

#2B Discrete
#abc=rcorr(HR_data)
chisq.test(HR_data$Gender, HR_data$Rank)
chisq.test(HR_data$Gender, HR_data$Dept)

#2C Scattered Plot
plot(HR_data$Gender, HR_data$Dept)

#2D Stacked BarChart - Check Which Attribute to select

# Stacked Barchart
count1 <- table(HR_data$Gender, HR_data$Dept)
count1
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(count1,  main="Attrition by Department", xlab="Department", ylab="Frequency", col = c("red", "grey", "blue"))
legend("topright",inset=c(-0.1,0), fill=c("red", "grey", "blue"), legend=rownames(count1), border = "grey", cex = 0.6)
# Stacked Percentage Barchart
prop1 <- prop.table(count1, margin=2) # calculate proportion based on freq in second column
prop1
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(prop1,  main="Attrition by Department by %", xlab="Department", ylab="Proportion", col = c("red", "grey", "blue"))
legend("topright",inset=c(-0.2,0), fill=c("red", "grey", "blue"), legend=rownames(count1), border = "grey", cex = 0.6)




#----------------------------------------------------------------
#Step 3 prediction

#3A Regression : Continuous
model <- Gender ~ Dept + Rank
result <-lm(model,HR_data)
summary(result)

#3Aii Logistic Regression : Discrete
model2 <- Gender ~ Dept + Rank
# Use a generalized linear model in the binomial family
result2 <- glm(model2,family=binomial,data=HR_data)
# Summarize the results
summary(result2)




#3B Classification Tree with rpart https://www.statmethods.net/advstats/cart.html
# grow tree 

#Discrete or Categorical
#fit <- rpart(Attrition ~ Age + Department + DistanceFromHome,
            # method="class", data=HR_data)
fit <- rpart(Gender ~ Dept + Rank, method="class", data=HR_data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Attrition")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree1.ps", 
     title = "Classification Tree for Attrition")

# prune the tree : CP complexity parameter
#pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

pfit<- prune(fit, cp=0.110204)
pfit
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Attrition")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "ptree.ps", 
     title = "Pruned Classification Tree for Attrition")


#Continuos
# grow tree 
fit2 <- rpart(Gender ~ Dept + Rank,method="anova", data=HR_data)

printcp(fit2) # display the results 
plotcp(fit2) # visualize cross-validation results 
summary(fit2) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit2) # visualize cross-validation results  	

# plot tree 
plot(fit2, uniform=TRUE, 
     main="Regression Tree for Attrition")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit2, file = "tree2.ps", 
     title = "Regression Tree for Attrition")

pfit2<- prune(fit2, cp=0.054066)
pfit2
# plot the pruned tree 
plot(pfit2, uniform=TRUE, main="Pruned Classification Tree for Attrition")
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)
post(pfit2, file = "ptree2.ps", 
     title = "Pruned Classification Tree for Attrition")

#pdf(fit, file="tree.pdf", width=6,height=4)

#dev.off()






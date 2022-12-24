setwd("D:/")

library(readxl)
Data <- read_excel("D:/LSTandvegetationindices.xlsx")
View(Data)
str(Data)

Data$LSTBinary<- as.factor(Data$LSTBinary) #dependent variable is in binary response- change it into factor
table(Data$LSTBinary)

# Data partition
set.seed(123)
Independent<- sample(2, nrow(Data), replace = TRUE, prob = c(0.7, 0.3)) # 70:30 data partition
train <- Data[Independent==1,]
test <- Data[Independent==2,]

# Random forests
install.packages("randomForest")
library(randomForest)

set.seed(222)
rf <- randomForest(LSTBinary~ ., data =train,
                   ntree= 300,
                   mtry = 2,
                   importance = TRUE,
                   proximity = TRUE) #random forests ntree and mtry were calculated from tune mtry- check the following codes
print(rf)
attributes(rf)  # check the attributes in rf
rf$confusion
rf$err.rate

# Prediction and confusion matrix- train data
install.packages("caret")
library(caret)
pred1 <- predict(rf, train)
head(pred1)                    #check prediction with train dataset
head(train$LSTBinary)          #compare with real data
confusionMatrix(pred1, train$LSTBinary)  # accuracy

#checking misclassification for training dataset
tabl <- table(pred1, train$LSTBinary) # creating confusion matrix
sum(diag(tabl))/sum(tabl)             # another way to check accuracy
1-sum(diag(tabl))/sum(tabl)            # checking misclassification error

# Prediction and confusion matrix- test data
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$LSTBinary)

# Method 2 to check # Prediction and model assessment including sensitivity and specifity
n <- table (train$LSTBinary)
n/sum(n)
tabl/colSums(tabl)  #for train dataset #it shows model accuracy for each response: 1 and 0: forested and non-forested area
tabl2/colSums(tabl2) #for test dataset #it shows model accuracy for each response: 1 and 0: forested and non-forested area

#checking misclassification for testing dataset
tabl2 <- table(pred2, test$LSTBinary) # creating confusion matrix
sum(diag(tabl2))/sum(tabl2)             # another way to check accuracy
1-sum(diag(tabl2))/sum(tabl2)            # checking misclassification error

# Error rate of Random Forest
plot(rf)

#Tune mtry
train <- as.data.frame(train)  #to consider train into data frame

t<- tuneRF(train[, -1], train[,1],
           stepFactor = 0.5,
           plot= TRUE,
           ntreeTry = 300,
           trace = TRUE,
           improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of nodes for the trees",
     col = "green")

# Variable importance
varImpPlot(rf,
           sort = T,
           main = " Variable importance")

importance(rf) # see the data in table
varUsed(rf)

# partial dependence plot 
partialPlot(rf, train, NDMI, "1") #enter 0, or 1, which are dependent variables

# Extract single tree 
getTree(rf, 1, labelVar = TRUE)

# Multi-dimesional scaling plot of proximity matrix 
MDSplot(rf, train$LSTBinary)


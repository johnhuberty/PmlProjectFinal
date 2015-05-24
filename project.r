

# John Huberty
# Practical machine learning
# May 2015
# Course project


#################################################
# Package install

#install.packages("gridExtra")
#install.packages("Hmisc")
#install.packages("AppliedPredictiveModeling")
#install.packages("caret")
#install.packages("ggplot2")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("randomForest")

library(gridExtra)
library(Hmisc)
library(AppliedPredictiveModeling)
library(caret)
library(ggplot2)
library(rattle)
library(rpart.plot)
library(randomForest)

#################################################
# get and extract data

set.seed(975)
setwd("C:/Users/jhuberty/Desktop/Coursera_data_science/9 Practical Machine Learning/Project")
pmltrain <- read.table("./pml-training.csv", header = TRUE, sep=',', na.strings=c("NA",""))
pmltest <- read.table("./pml-testing.csv", header = TRUE, sep=',', na.strings=c("NA",""))

#create factor variable
pmltrain$classe <- as.factor(pmltrain$classe)

# Drop columns with NA
pmltrain <- pmltrain[ , colSums(is.na(pmltrain)) == 0]
pmltest <- pmltest[ , colSums(is.na(pmltest)) == 0]

# Drop first 7 columns as they are variables that do not predict exercise outcome
pmltrain <- pmltrain[ -c(1:7)]
pmltest <- pmltest[ -c(1:7)]





#create test names
trainnames <-colnames(pmltrain)
testnames <-colnames(pmltest)

#verify same columns names are equal
all.equal(trainnames[1:length(trainnames)-1], testnames[1:length(testnames)-1])

#partition training data set
pmlpartition = createDataPartition(pmltrain$classe, p = 3/4, list = FALSE)
pmltraining <- pmltrain[pmlpartition,]
pmltrainingtest <- pmltrain[-pmlpartition,]

#exercise <- unique(pmltraining$classe)

################################################
# Calculate prediction model using tree algorithm
# modfit <- train(classe ~ ., data = pmltraining, method = "rpart")
# print(modfit$finalModel)
# plot(modfit$finalModel, uniform = TRUE, main = "Classification Tree",)
# text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)
# 
# 
# fancyRpartPlot(modfit$finalModel)
# predict(modfit, newdata = pmltesting)


################################################
# Calculate prediction model using random forests

if(file.exists("m2.rda")) {
  # Load computationally intensive file 
  load("m2.rda")  
} else {
  ## re(fit) model
  
  #defining training control
  train_control <- trainControl(method="CV", number = 10)
  #Train the model
  modfit2 <- train(classe ~ ., data = pmltraining, trControl = train_control, method = "rf")
  save(modfit2, file = "m2.rda")
}

#Make prediction
predtrain <- predict(modfit2,pmltraining)
predtraintest <- predict(modfit2,pmltrainingtest)

#summarizer results
confusionMatrix(predtrain, pmltraining$classe)
confusionMatrix(predtraintest, pmltrainingtest$classe)

predtest <- predict(modfit2, pmltest)

# Create a text file
answers <- predtest
  
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
pml_write_files(answers)



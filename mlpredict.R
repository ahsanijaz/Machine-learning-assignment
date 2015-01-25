library(caret)
library(dplyr)
library(plyr)

trainingData<-read.csv("Personal/RRpro1//MLproject/pml-training.csv",na.strings=c("","NA","#DIV/0!"))
testingData <- read.csv("Personal/RRpro1//MLproject/pml-testing.csv",na.strings=c("","NA","#DIV/0!"))

nzv <- nearZeroVar(trainingData)
filteredTrainingData <- trainingData[, -nzv]
testingData <- testingData[,-nzv]
dim(filteredTrainingData)


filteredTrainingData <- filteredTrainingData %>%
  select(-user_name,-raw_timestamp_part_1,-raw_timestamp_part_2,-cvtd_timestamp,-num_window,-X)
filteredTrainingData1 <- filteredTrainingData[,colSums(is.na(filteredTrainingData))==0]

testingData <- testingData %>%
  select(-user_name,-raw_timestamp_part_1,-raw_timestamp_part_2,-cvtd_timestamp,-num_window,-X)
testingData <- testingData[,colSums(is.na(filteredTrainingData))==0]

for(i in 1:(length(filteredTrainingData1)-1))
  filteredTrainingData1[,i] <- as.numeric(filteredTrainingData1[,i])

for(i in 1:(length(testingData)-1))
  testingData[,i] <- as.numeric(testingData[,i])

inTrain<-createDataPartition(y=filteredTrainingData1$classe,p=0.5,list=FALSE)
training<-filteredTrainingData1[inTrain,]
test<-filteredTrainingData1[-inTrain,]
preProc <- preProcess(training[,-length(training)],method="pca",thresh = 0.90)

trainPC <- predict(preProc,training[,-length(training)])
modelFit <- train(training$classe ~ .,method="rf",data=trainPC,trControl=trainControl(method="cv",number = 5),prox=TRUE)


testPC <- predict(preProc,test[,-length(test)])
modFit.RF.PCA.predict<-predict(modelFit$finalModel,testPC, type="class")

confusionMatrix(modFit.RF.PCA.predict,test$classe)

save(modelFit,file = "Personal/RRpro1/MLproject/modelRFPCA.RData")

testNPC <- predict(preProc,testingData[,-length(testingData)])
modFit.RF.PCA.predict<-predict(modelFit$finalModel,testNPC, type="class")



pml_write_files(predicttest)
confusionMatrix(modFit.RF.PCA.predict,testingData$classe)


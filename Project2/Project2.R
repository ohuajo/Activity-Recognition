#####################################
##                                 ##
##    PHASE 1: TrainingTestData    ##
##                                 ##
#####################################

# Perform PCA. 
# Prepare data from features, both eating and non eating
pcaInput <- rbind(pcaInput_eating[c(2:6)], pcaInput_non_eating[c(2:6)])
# Rename columns to reflect features
names(pcaInput) <- (c("Mean", "Median", "STD", "Max", "ICA"))
# do pca
eat_pca <- princomp(pcaInput, cor = TRUE, scores = TRUE)
# Summary information of the pca to know contribution of each component and to determine which PCs to use
summary(eat_pca)
# Generate new features matrix
newFeatures <- eat_pca$scores
newFeatureMatrix <- eat_pca$scores[,1:3] %*% t(eat_pca$loadings[,1:3])
newFeatureMatrix <- scale(newFeatureMatrix, center = -eat_pca$center, scale = TRUE)
# Get test and traing data
pcaClean <- data.frame("Eat1Noneat0"=0, newFeatureMatrix)
z <- dim(newFeatureMatrix)[1]*0.5
pcaClean$Eat1Noneat0[1:z] = 1
training_eat_end <- round(dim(newFeatureMatrix)[1]*0.3)
testing_eat_start <- training_eat_end + 1
testing_eat_end <- z
training_noneat_start <- z + 1
training_noneat_end <- round(dim(newFeatureMatrix)[1]*0.8)
testing_noneat_start <- training_noneat_end + 1
testing_noneat_end <- dim(newFeatureMatrix)[1]
trainingdata <- rbind(pcaClean[1:training_eat_end, ], pcaClean[training_noneat_start:training_noneat_end,])
testdata <- rbind(pcaClean[testing_eat_start:testing_eat_end, ], pcaClean[testing_noneat_start:testing_noneat_end,])
write.table(trainingdata, file = "training25.csv", sep = ",", row.names=FALSE)
write.table(testdata, file = "test25.csv", sep = ",", row.names=FALSE)



#######################################
##                                   ##
##    PHASE 1: Decision Tree & SVM   ##
##                                   ##
#######################################

#Make dataframe to store result of matrices
MetricsProject2 <- data.frame(matrix(0, 30, 14))
row.names(MetricsProject2) <- c("user09", "user10", "user11", "user12", "user13", "user14", "user16", "user17", "user18", "user19", "user21",
                                "user22", "user23", "user24", "user25", "user26", "user27", "user28", "user29", "user30", "user31",
                                "user32", "user33", "user34", "user36", "user37", "user38", "user39", "user40", "user41")
colnames(MetricsProject2) <- c("TP_dt", "TN_dt", "FP_dt", "FN_dt", "DTprecision", "DTrecall", "DTf1score", 
                               "TP_s", "TN_s", "FP_s", "FN_s", "sprecision", "srecall", "sf1score")


###### Decision Tree ######
###########################
#Load rpart
library(rpart)
#Get previously gnearated training and testing data
test09 <- read.csv("TrainingTestData/test09.csv")
train09 <- read.csv("TrainingTestData/training09.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train09, method = "class")
#test model
p <- predict(dtm, test09, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test09[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision09 <- TP/(TP+FP)
#Recall
DTrecall09 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score09 <- 2*(DTrecall09 * DTprecision09) / (DTrecall09 + DTprecision09)
MetricsProject2["user09", "TP_dt"] <- TP
MetricsProject2["user09", "TN_dt"] <- TN
MetricsProject2["user09", "FP_dt"] <- FP
MetricsProject2["user09", "FN_dt"] <- FN
MetricsProject2["user09", "DTprecision"] <- DTprecision09
MetricsProject2["user09", "DTrecall"] <- DTrecall09
MetricsProject2["user09", "DTf1score"] <- DTf1score09
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Load e1071
library("e1071")
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train09, type = "C-classification")
#test model
p <- predict(svmm, test09, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test09[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision09 <- TP/(TP+FP)
#Recall
SVMrecall09 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score09 <- 2*(SVMrecall09 * SVMprecision09) / (SVMrecall09 + SVMprecision09)
MetricsProject2["user09", "TP_s"] <- TP
MetricsProject2["user09", "TN_s"] <- TN
MetricsProject2["user09", "FP_s"] <- FP
MetricsProject2["user09", "FN_s"] <- FN
MetricsProject2["user09", "sprecision"] <- SVMprecision09
MetricsProject2["user09", "srecall"] <- SVMrecall09
MetricsProject2["user09", "sf1score"] <- SVMf1score09
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test10 <- read.csv("TrainingTestData/test10.csv")
train10 <- read.csv("TrainingTestData/training10.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train10, method = "class")
#test model
p <- predict(dtm, test10, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test10[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision10 <- TP/(TP+FP)
#Recall
DTrecall10 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score10 <- 2*(DTrecall10 * DTprecision10) / (DTrecall10 + DTprecision10)
MetricsProject2["user10", "TP_dt"] <- TP
MetricsProject2["user10", "TN_dt"] <- TN
MetricsProject2["user10", "FP_dt"] <- FP
MetricsProject2["user10", "FN_dt"] <- FN
MetricsProject2["user10", "DTprecision"] <- DTprecision10
MetricsProject2["user10", "DTrecall"] <- DTrecall10
MetricsProject2["user10", "DTf1score"] <- DTf1score10
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train10, type = "C-classification")
#test model
p <- predict(svmm, test10, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test10[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision10 <- TP/(TP+FP)
#Recall
SVMrecall10 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score10 <- 2*(SVMrecall10 * SVMprecision10) / (SVMrecall10 + SVMprecision10)
MetricsProject2["user10", "TP_s"] <- TP
MetricsProject2["user10", "TN_s"] <- TN
MetricsProject2["user10", "FP_s"] <- FP
MetricsProject2["user10", "FN_s"] <- FN
MetricsProject2["user10", "sprecision"] <- SVMprecision10
MetricsProject2["user10", "srecall"] <- SVMrecall10
MetricsProject2["user10", "sf1score"] <- SVMf1score10
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test11 <- read.csv("TrainingTestData/test11.csv")
train11 <- read.csv("TrainingTestData/training11.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train11, method = "class")
#test model
p <- predict(dtm, test11, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test11[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision11 <- TP/(TP+FP)
#Recall
DTrecall11 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score11 <- 2*(DTrecall11 * DTprecision11) / (DTrecall11 + DTprecision11)
MetricsProject2["user11", "TP_dt"] <- TP
MetricsProject2["user11", "TN_dt"] <- TN
MetricsProject2["user11", "FP_dt"] <- FP
MetricsProject2["user11", "FN_dt"] <- FN
MetricsProject2["user11", "DTprecision"] <- DTprecision11
MetricsProject2["user11", "DTrecall"] <- DTrecall11
MetricsProject2["user11", "DTf1score"] <- DTf1score11
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train11, type = "C-classification")
#test model
p <- predict(svmm, test11, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test11[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision11 <- TP/(TP+FP)
#Recall
SVMrecall11 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score11 <- 2*(SVMrecall11 * SVMprecision11) / (SVMrecall11 + SVMprecision11)
MetricsProject2["user11", "TP_s"] <- TP
MetricsProject2["user11", "TN_s"] <- TN
MetricsProject2["user11", "FP_s"] <- FP
MetricsProject2["user11", "FN_s"] <- FN
MetricsProject2["user11", "sprecision"] <- SVMprecision11
MetricsProject2["user11", "srecall"] <- SVMrecall11
MetricsProject2["user11", "sf1score"] <- SVMf1score11
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test12 <- read.csv("TrainingTestData/test12.csv")
train12 <- read.csv("TrainingTestData/training12.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train12, method = "class")
#test model
p <- predict(dtm, test12, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test12[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision12 <- TP/(TP+FP)
#Recall
DTrecall12 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score12 <- 2*(DTrecall12 * DTprecision12) / (DTrecall12 + DTprecision12)
MetricsProject2["user12", "TP_dt"] <- TP
MetricsProject2["user12", "TN_dt"] <- TN
MetricsProject2["user12", "FP_dt"] <- FP
MetricsProject2["user12", "FN_dt"] <- FN
MetricsProject2["user12", "DTprecision"] <- DTprecision12
MetricsProject2["user12", "DTrecall"] <- DTrecall12
MetricsProject2["user12", "DTf1score"] <- DTf1score12
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train12, type = "C-classification")
#test model
p <- predict(svmm, test12, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test12[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision12 <- TP/(TP+FP)
#Recall
SVMrecall12 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score12 <- 2*(SVMrecall12 * SVMprecision12) / (SVMrecall12 + SVMprecision12)
MetricsProject2["user12", "TP_s"] <- TP
MetricsProject2["user12", "TN_s"] <- TN
MetricsProject2["user12", "FP_s"] <- FP
MetricsProject2["user12", "FN_s"] <- FN
MetricsProject2["user12", "sprecision"] <- SVMprecision12
MetricsProject2["user12", "srecall"] <- SVMrecall12
MetricsProject2["user12", "sf1score"] <- SVMf1score12
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test13 <- read.csv("TrainingTestData/test13.csv")
train13 <- read.csv("TrainingTestData/training13.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train13, method = "class")
#test model
p <- predict(dtm, test13, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test13[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision13 <- TP/(TP+FP)
#Recall
DTrecall13 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score13 <- 2*(DTrecall13 * DTprecision13) / (DTrecall13 + DTprecision13)
MetricsProject2["user13", "TP_dt"] <- TP
MetricsProject2["user13", "TN_dt"] <- TN
MetricsProject2["user13", "FP_dt"] <- FP
MetricsProject2["user13", "FN_dt"] <- FN
MetricsProject2["user13", "DTprecision"] <- DTprecision13
MetricsProject2["user13", "DTrecall"] <- DTrecall13
MetricsProject2["user13", "DTf1score"] <- DTf1score13
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train13, type = "C-classification")
#test model
p <- predict(svmm, test13, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test13[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision13 <- TP/(TP+FP)
#Recall
SVMrecall13 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score13 <- 2*(SVMrecall13 * SVMprecision13) / (SVMrecall13 + SVMprecision13)
MetricsProject2["user13", "TP_s"] <- TP
MetricsProject2["user13", "TN_s"] <- TN
MetricsProject2["user13", "FP_s"] <- FP
MetricsProject2["user13", "FN_s"] <- FN
MetricsProject2["user13", "sprecision"] <- SVMprecision13
MetricsProject2["user13", "srecall"] <- SVMrecall13
MetricsProject2["user13", "sf1score"] <- SVMf1score13
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test14 <- read.csv("TrainingTestData/test14.csv")
train14 <- read.csv("TrainingTestData/training14.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train14, method = "class")
#test model
p <- predict(dtm, test14, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test14[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision14 <- TP/(TP+FP)
#Recall
DTrecall14 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score14 <- 2*(DTrecall14 * DTprecision14) / (DTrecall14 + DTprecision14)
MetricsProject2["user14", "TP_dt"] <- TP
MetricsProject2["user14", "TN_dt"] <- TN
MetricsProject2["user14", "FP_dt"] <- FP
MetricsProject2["user14", "FN_dt"] <- FN
MetricsProject2["user14", "DTprecision"] <- DTprecision14
MetricsProject2["user14", "DTrecall"] <- DTrecall14
MetricsProject2["user14", "DTf1score"] <- DTf1score14
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train14, type = "C-classification")
#test model
p <- predict(svmm, test14, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test14[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision14 <- TP/(TP+FP)
#Recall
SVMrecall14 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score14 <- 2*(SVMrecall14 * SVMprecision14) / (SVMrecall14 + SVMprecision14)
MetricsProject2["user14", "TP_s"] <- TP
MetricsProject2["user14", "TN_s"] <- TN
MetricsProject2["user14", "FP_s"] <- FP
MetricsProject2["user14", "FN_s"] <- FN
MetricsProject2["user14", "sprecision"] <- SVMprecision14
MetricsProject2["user14", "srecall"] <- SVMrecall14
MetricsProject2["user14", "sf1score"] <- SVMf1score14
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test16 <- read.csv("TrainingTestData/test16.csv")
train16 <- read.csv("TrainingTestData/training16.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train16, method = "class")
#test model
p <- predict(dtm, test16, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test16[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision16 <- TP/(TP+FP)
#Recall
DTrecall16 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score16 <- 2*(DTrecall16 * DTprecision16) / (DTrecall16 + DTprecision16)
MetricsProject2["user16", "TP_dt"] <- TP
MetricsProject2["user16", "TN_dt"] <- TN
MetricsProject2["user16", "FP_dt"] <- FP
MetricsProject2["user16", "FN_dt"] <- FN
MetricsProject2["user16", "DTprecision"] <- DTprecision16
MetricsProject2["user16", "DTrecall"] <- DTrecall16
MetricsProject2["user16", "DTf1score"] <- DTf1score16
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train16, type = "C-classification")
#test model
p <- predict(svmm, test16, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test16[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision16 <- TP/(TP+FP)
#Recall
SVMrecall16 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score16 <- 2*(SVMrecall16 * SVMprecision16) / (SVMrecall16 + SVMprecision16)
MetricsProject2["user16", "TP_s"] <- TP
MetricsProject2["user16", "TN_s"] <- TN
MetricsProject2["user16", "FP_s"] <- FP
MetricsProject2["user16", "FN_s"] <- FN
MetricsProject2["user16", "sprecision"] <- SVMprecision16
MetricsProject2["user16", "srecall"] <- SVMrecall16
MetricsProject2["user16", "sf1score"] <- SVMf1score16
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test17 <- read.csv("TrainingTestData/test17.csv")
train17 <- read.csv("TrainingTestData/training17.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train17, method = "class")
#test model
p <- predict(dtm, test17, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test17[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision17 <- TP/(TP+FP)
#Recall
DTrecall17 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score17 <- 2*(DTrecall17 * DTprecision17) / (DTrecall17 + DTprecision17)
MetricsProject2["user17", "TP_dt"] <- TP
MetricsProject2["user17", "TN_dt"] <- TN
MetricsProject2["user17", "FP_dt"] <- FP
MetricsProject2["user17", "FN_dt"] <- FN
MetricsProject2["user17", "DTprecision"] <- DTprecision17
MetricsProject2["user17", "DTrecall"] <- DTrecall17
MetricsProject2["user17", "DTf1score"] <- DTf1score17
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train17, type = "C-classification")
#test model
p <- predict(svmm, test17, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test17[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision17 <- TP/(TP+FP)
#Recall
SVMrecall17 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score17 <- 2*(SVMrecall17 * SVMprecision17) / (SVMrecall17 + SVMprecision17)
MetricsProject2["user17", "TP_s"] <- TP
MetricsProject2["user17", "TN_s"] <- TN
MetricsProject2["user17", "FP_s"] <- FP
MetricsProject2["user17", "FN_s"] <- FN
MetricsProject2["user17", "sprecision"] <- SVMprecision17
MetricsProject2["user17", "srecall"] <- SVMrecall17
MetricsProject2["user17", "sf1score"] <- SVMf1score17
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test18 <- read.csv("TrainingTestData/test18.csv")
train18 <- read.csv("TrainingTestData/training18.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train18, method = "class")
#test model
p <- predict(dtm, test18, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test18[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision18 <- TP/(TP+FP)
#Recall
DTrecall18 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score18 <- 2*(DTrecall18 * DTprecision18) / (DTrecall18 + DTprecision18)
MetricsProject2["user18", "TP_dt"] <- TP
MetricsProject2["user18", "TN_dt"] <- TN
MetricsProject2["user18", "FP_dt"] <- FP
MetricsProject2["user18", "FN_dt"] <- FN
MetricsProject2["user18", "DTprecision"] <- DTprecision18
MetricsProject2["user18", "DTrecall"] <- DTrecall18
MetricsProject2["user18", "DTf1score"] <- DTf1score18
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train18, type = "C-classification")
#test model
p <- predict(svmm, test18, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test18[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision18 <- TP/(TP+FP)
#Recall
SVMrecall18 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score18 <- 2*(SVMrecall18 * SVMprecision18) / (SVMrecall18 + SVMprecision18)
MetricsProject2["user18", "TP_s"] <- TP
MetricsProject2["user18", "TN_s"] <- TN
MetricsProject2["user18", "FP_s"] <- FP
MetricsProject2["user18", "FN_s"] <- FN
MetricsProject2["user18", "sprecision"] <- SVMprecision18
MetricsProject2["user18", "srecall"] <- SVMrecall18
MetricsProject2["user18", "sf1score"] <- SVMf1score18
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test19 <- read.csv("TrainingTestData/test19.csv")
train19 <- read.csv("TrainingTestData/training19.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train19, method = "class")
#test model
p <- predict(dtm, test19, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test19[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision19 <- TP/(TP+FP)
#Recall
DTrecall19 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score19 <- 2*(DTrecall19 * DTprecision19) / (DTrecall19 + DTprecision19)
MetricsProject2["user19", "TP_dt"] <- TP
MetricsProject2["user19", "TN_dt"] <- TN
MetricsProject2["user19", "FP_dt"] <- FP
MetricsProject2["user19", "FN_dt"] <- FN
MetricsProject2["user19", "DTprecision"] <- DTprecision19
MetricsProject2["user19", "DTrecall"] <- DTrecall19
MetricsProject2["user19", "DTf1score"] <- DTf1score19
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train19, type = "C-classification")
#test model
p <- predict(svmm, test19, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test19[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision19 <- TP/(TP+FP)
#Recall
SVMrecall19 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score19 <- 2*(SVMrecall19 * SVMprecision19) / (SVMrecall19 + SVMprecision19)
MetricsProject2["user19", "TP_s"] <- TP
MetricsProject2["user19", "TN_s"] <- TN
MetricsProject2["user19", "FP_s"] <- FP
MetricsProject2["user19", "FN_s"] <- FN
MetricsProject2["user19", "sprecision"] <- SVMprecision19
MetricsProject2["user19", "srecall"] <- SVMrecall19
MetricsProject2["user19", "sf1score"] <- SVMf1score19
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test21 <- read.csv("TrainingTestData/test21.csv")
train21 <- read.csv("TrainingTestData/training21.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train21, method = "class")
#test model
p <- predict(dtm, test21, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test21[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision21 <- TP/(TP+FP)
#Recall
DTrecall21 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score21 <- 2*(DTrecall21 * DTprecision21) / (DTrecall21 + DTprecision21)
MetricsProject2["user21", "TP_dt"] <- TP
MetricsProject2["user21", "TN_dt"] <- TN
MetricsProject2["user21", "FP_dt"] <- FP
MetricsProject2["user21", "FN_dt"] <- FN
MetricsProject2["user21", "DTprecision"] <- DTprecision21
MetricsProject2["user21", "DTrecall"] <- DTrecall21
MetricsProject2["user21", "DTf1score"] <- DTf1score21
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train21, type = "C-classification")
#test model
p <- predict(svmm, test21, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test21[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision21 <- TP/(TP+FP)
#Recall
SVMrecall21 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score21 <- 2*(SVMrecall21 * SVMprecision21) / (SVMrecall21 + SVMprecision21)
MetricsProject2["user21", "TP_s"] <- TP
MetricsProject2["user21", "TN_s"] <- TN
MetricsProject2["user21", "FP_s"] <- FP
MetricsProject2["user21", "FN_s"] <- FN
MetricsProject2["user21", "sprecision"] <- SVMprecision21
MetricsProject2["user21", "srecall"] <- SVMrecall21
MetricsProject2["user21", "sf1score"] <- SVMf1score21
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test22 <- read.csv("TrainingTestData/test22.csv")
train22 <- read.csv("TrainingTestData/training22.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train22, method = "class")
#test model
p <- predict(dtm, test22, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test22[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision22 <- TP/(TP+FP)
#Recall
DTrecall22 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score22 <- 2*(DTrecall22 * DTprecision22) / (DTrecall22 + DTprecision22)
MetricsProject2["user22", "TP_dt"] <- TP
MetricsProject2["user22", "TN_dt"] <- TN
MetricsProject2["user22", "FP_dt"] <- FP
MetricsProject2["user22", "FN_dt"] <- FN
MetricsProject2["user22", "DTprecision"] <- DTprecision22
MetricsProject2["user22", "DTrecall"] <- DTrecall22
MetricsProject2["user22", "DTf1score"] <- DTf1score22
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train22, type = "C-classification")
#test model
p <- predict(svmm, test22, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test22[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision22 <- TP/(TP+FP)
#Recall
SVMrecall22 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score22 <- 2*(SVMrecall22 * SVMprecision22) / (SVMrecall22 + SVMprecision22)
MetricsProject2["user22", "TP_s"] <- TP
MetricsProject2["user22", "TN_s"] <- TN
MetricsProject2["user22", "FP_s"] <- FP
MetricsProject2["user22", "FN_s"] <- FN
MetricsProject2["user22", "sprecision"] <- SVMprecision22
MetricsProject2["user22", "srecall"] <- SVMrecall22
MetricsProject2["user22", "sf1score"] <- SVMf1score22
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test23 <- read.csv("TrainingTestData/test23.csv")
train23 <- read.csv("TrainingTestData/training23.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train23, method = "class")
#test model
p <- predict(dtm, test23, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test23[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision23 <- TP/(TP+FP)
#Recall
DTrecall23 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score23 <- 2*(DTrecall23 * DTprecision23) / (DTrecall23 + DTprecision23)
MetricsProject2["user23", "TP_dt"] <- TP
MetricsProject2["user23", "TN_dt"] <- TN
MetricsProject2["user23", "FP_dt"] <- FP
MetricsProject2["user23", "FN_dt"] <- FN
MetricsProject2["user23", "DTprecision"] <- DTprecision23
MetricsProject2["user23", "DTrecall"] <- DTrecall23
MetricsProject2["user23", "DTf1score"] <- DTf1score23
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train23, type = "C-classification")
#test model
p <- predict(svmm, test23, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test23[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision23 <- TP/(TP+FP)
#Recall
SVMrecall23 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score23 <- 2*(SVMrecall23 * SVMprecision23) / (SVMrecall23 + SVMprecision23)
MetricsProject2["user23", "TP_s"] <- TP
MetricsProject2["user23", "TN_s"] <- TN
MetricsProject2["user23", "FP_s"] <- FP
MetricsProject2["user23", "FN_s"] <- FN
MetricsProject2["user23", "sprecision"] <- SVMprecision23
MetricsProject2["user23", "srecall"] <- SVMrecall23
MetricsProject2["user23", "sf1score"] <- SVMf1score23
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test24 <- read.csv("TrainingTestData/test24.csv")
train24 <- read.csv("TrainingTestData/training24.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train24, method = "class")
#test model
p <- predict(dtm, test24, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test24[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision24 <- TP/(TP+FP)
#Recall
DTrecall24 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score24 <- 2*(DTrecall24 * DTprecision24) / (DTrecall24 + DTprecision24)
MetricsProject2["user24", "TP_dt"] <- TP
MetricsProject2["user24", "TN_dt"] <- TN
MetricsProject2["user24", "FP_dt"] <- FP
MetricsProject2["user24", "FN_dt"] <- FN
MetricsProject2["user24", "DTprecision"] <- DTprecision24
MetricsProject2["user24", "DTrecall"] <- DTrecall24
MetricsProject2["user24", "DTf1score"] <- DTf1score24
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train24, type = "C-classification")
#test model
p <- predict(svmm, test24, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test24[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision24 <- TP/(TP+FP)
#Recall
SVMrecall24 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score24 <- 2*(SVMrecall24 * SVMprecision24) / (SVMrecall24 + SVMprecision24)
MetricsProject2["user24", "TP_s"] <- TP
MetricsProject2["user24", "TN_s"] <- TN
MetricsProject2["user24", "FP_s"] <- FP
MetricsProject2["user24", "FN_s"] <- FN
MetricsProject2["user24", "sprecision"] <- SVMprecision24
MetricsProject2["user24", "srecall"] <- SVMrecall24
MetricsProject2["user24", "sf1score"] <- SVMf1score24
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test25 <- read.csv("TrainingTestData/test25.csv")
train25 <- read.csv("TrainingTestData/training25.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train25, method = "class")
#test model
p <- predict(dtm, test25, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test25[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision25 <- TP/(TP+FP)
#Recall
DTrecall25 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score25 <- 2*(DTrecall25 * DTprecision25) / (DTrecall25 + DTprecision25)
MetricsProject2["user25", "TP_dt"] <- TP
MetricsProject2["user25", "TN_dt"] <- TN
MetricsProject2["user25", "FP_dt"] <- FP
MetricsProject2["user25", "FN_dt"] <- FN
MetricsProject2["user25", "DTprecision"] <- DTprecision25
MetricsProject2["user25", "DTrecall"] <- DTrecall25
MetricsProject2["user25", "DTf1score"] <- DTf1score25
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train25, type = "C-classification")
#test model
p <- predict(svmm, test25, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test25[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision25 <- TP/(TP+FP)
#Recall
SVMrecall25 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score25 <- 2*(SVMrecall25 * SVMprecision25) / (SVMrecall25 + SVMprecision25)
MetricsProject2["user25", "TP_s"] <- TP
MetricsProject2["user25", "TN_s"] <- TN
MetricsProject2["user25", "FP_s"] <- FP
MetricsProject2["user25", "FN_s"] <- FN
MetricsProject2["user25", "sprecision"] <- SVMprecision25
MetricsProject2["user25", "srecall"] <- SVMrecall25
MetricsProject2["user25", "sf1score"] <- SVMf1score25
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test26 <- read.csv("TrainingTestData/test26.csv")
train26 <- read.csv("TrainingTestData/training26.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train26, method = "class")
#test model
p <- predict(dtm, test26, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test26[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision26 <- TP/(TP+FP)
#Recall
DTrecall26 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score26 <- 2*(DTrecall26 * DTprecision26) / (DTrecall26 + DTprecision26)
MetricsProject2["user26", "TP_dt"] <- TP
MetricsProject2["user26", "TN_dt"] <- TN
MetricsProject2["user26", "FP_dt"] <- FP
MetricsProject2["user26", "FN_dt"] <- FN
MetricsProject2["user26", "DTprecision"] <- DTprecision26
MetricsProject2["user26", "DTrecall"] <- DTrecall26
MetricsProject2["user26", "DTf1score"] <- DTf1score26
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train26, type = "C-classification")
#test model
p <- predict(svmm, test26, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test26[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision26 <- TP/(TP+FP)
#Recall
SVMrecall26 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score26 <- 2*(SVMrecall26 * SVMprecision26) / (SVMrecall26 + SVMprecision26)
MetricsProject2["user26", "TP_s"] <- TP
MetricsProject2["user26", "TN_s"] <- TN
MetricsProject2["user26", "FP_s"] <- FP
MetricsProject2["user26", "FN_s"] <- FN
MetricsProject2["user26", "sprecision"] <- SVMprecision26
MetricsProject2["user26", "srecall"] <- SVMrecall26
MetricsProject2["user26", "sf1score"] <- SVMf1score26
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test27 <- read.csv("TrainingTestData/test27.csv")
train27 <- read.csv("TrainingTestData/training27.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train27, method = "class")
#test model
p <- predict(dtm, test27, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test27[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision27 <- TP/(TP+FP)
#Recall
DTrecall27 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score27 <- 2*(DTrecall27 * DTprecision27) / (DTrecall27 + DTprecision27)
MetricsProject2["user27", "TP_dt"] <- TP
MetricsProject2["user27", "TN_dt"] <- TN
MetricsProject2["user27", "FP_dt"] <- FP
MetricsProject2["user27", "FN_dt"] <- FN
MetricsProject2["user27", "DTprecision"] <- DTprecision27
MetricsProject2["user27", "DTrecall"] <- DTrecall27
MetricsProject2["user27", "DTf1score"] <- DTf1score27
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train27, type = "C-classification")
#test model
p <- predict(svmm, test27, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test27[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision27 <- TP/(TP+FP)
#Recall
SVMrecall27 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score27 <- 2*(SVMrecall27 * SVMprecision27) / (SVMrecall27 + SVMprecision27)
MetricsProject2["user27", "TP_s"] <- TP
MetricsProject2["user27", "TN_s"] <- TN
MetricsProject2["user27", "FP_s"] <- FP
MetricsProject2["user27", "FN_s"] <- FN
MetricsProject2["user27", "sprecision"] <- SVMprecision27
MetricsProject2["user27", "srecall"] <- SVMrecall27
MetricsProject2["user27", "sf1score"] <- SVMf1score27
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test28 <- read.csv("TrainingTestData/test28.csv")
train28 <- read.csv("TrainingTestData/training28.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train28, method = "class")
#test model
p <- predict(dtm, test28, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test28[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision28 <- TP/(TP+FP)
#Recall
DTrecall28 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score28 <- 2*(DTrecall28 * DTprecision28) / (DTrecall28 + DTprecision28)
MetricsProject2["user28", "TP_dt"] <- TP
MetricsProject2["user28", "TN_dt"] <- TN
MetricsProject2["user28", "FP_dt"] <- FP
MetricsProject2["user28", "FN_dt"] <- FN
MetricsProject2["user28", "DTprecision"] <- DTprecision28
MetricsProject2["user28", "DTrecall"] <- DTrecall28
MetricsProject2["user28", "DTf1score"] <- DTf1score28
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train28, type = "C-classification")
#test model
p <- predict(svmm, test28, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test28[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision28 <- TP/(TP+FP)
#Recall
SVMrecall28 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score28 <- 2*(SVMrecall28 * SVMprecision28) / (SVMrecall28 + SVMprecision28)
MetricsProject2["user28", "TP_s"] <- TP
MetricsProject2["user28", "TN_s"] <- TN
MetricsProject2["user28", "FP_s"] <- FP
MetricsProject2["user28", "FN_s"] <- FN
MetricsProject2["user28", "sprecision"] <- SVMprecision28
MetricsProject2["user28", "srecall"] <- SVMrecall28
MetricsProject2["user28", "sf1score"] <- SVMf1score28
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test29 <- read.csv("TrainingTestData/test29.csv")
train29 <- read.csv("TrainingTestData/training29.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train29, method = "class")
#test model
p <- predict(dtm, test29, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test29[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision29 <- TP/(TP+FP)
#Recall
DTrecall29 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score29 <- 2*(DTrecall29 * DTprecision29) / (DTrecall29 + DTprecision29)
MetricsProject2["user29", "TP_dt"] <- TP
MetricsProject2["user29", "TN_dt"] <- TN
MetricsProject2["user29", "FP_dt"] <- FP
MetricsProject2["user29", "FN_dt"] <- FN
MetricsProject2["user29", "DTprecision"] <- DTprecision29
MetricsProject2["user29", "DTrecall"] <- DTrecall29
MetricsProject2["user29", "DTf1score"] <- DTf1score29
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train29, type = "C-classification")
#test model
p <- predict(svmm, test29, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test29[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision29 <- TP/(TP+FP)
#Recall
SVMrecall29 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score29 <- 2*(SVMrecall29 * SVMprecision29) / (SVMrecall29 + SVMprecision29)
MetricsProject2["user29", "TP_s"] <- TP
MetricsProject2["user29", "TN_s"] <- TN
MetricsProject2["user29", "FP_s"] <- FP
MetricsProject2["user29", "FN_s"] <- FN
MetricsProject2["user29", "sprecision"] <- SVMprecision29
MetricsProject2["user29", "srecall"] <- SVMrecall29
MetricsProject2["user29", "sf1score"] <- SVMf1score29
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test30 <- read.csv("TrainingTestData/test30.csv")
train30 <- read.csv("TrainingTestData/training30.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train30, method = "class")
#test model
p <- predict(dtm, test30, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test30[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision30 <- TP/(TP+FP)
#Recall
DTrecall30 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score30 <- 2*(DTrecall30 * DTprecision30) / (DTrecall30 + DTprecision30)
MetricsProject2["user30", "TP_dt"] <- TP
MetricsProject2["user30", "TN_dt"] <- TN
MetricsProject2["user30", "FP_dt"] <- FP
MetricsProject2["user30", "FN_dt"] <- FN
MetricsProject2["user30", "DTprecision"] <- DTprecision30
MetricsProject2["user30", "DTrecall"] <- DTrecall30
MetricsProject2["user30", "DTf1score"] <- DTf1score30
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train30, type = "C-classification")
#test model
p <- predict(svmm, test30, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test30[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision30 <- TP/(TP+FP)
#Recall
SVMrecall30 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score30 <- 2*(SVMrecall30 * SVMprecision30) / (SVMrecall30 + SVMprecision30)
MetricsProject2["user30", "TP_s"] <- TP
MetricsProject2["user30", "TN_s"] <- TN
MetricsProject2["user30", "FP_s"] <- FP
MetricsProject2["user30", "FN_s"] <- FN
MetricsProject2["user30", "sprecision"] <- SVMprecision30
MetricsProject2["user30", "srecall"] <- SVMrecall30
MetricsProject2["user30", "sf1score"] <- SVMf1score30
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test31 <- read.csv("TrainingTestData/test31.csv")
train31 <- read.csv("TrainingTestData/training31.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train31, method = "class")
#test model
p <- predict(dtm, test31, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test31[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision31 <- TP/(TP+FP)
#Recall
DTrecall31 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score31 <- 2*(DTrecall31 * DTprecision31) / (DTrecall31 + DTprecision31)
MetricsProject2["user31", "TP_dt"] <- TP
MetricsProject2["user31", "TN_dt"] <- TN
MetricsProject2["user31", "FP_dt"] <- FP
MetricsProject2["user31", "FN_dt"] <- FN
MetricsProject2["user31", "DTprecision"] <- DTprecision31
MetricsProject2["user31", "DTrecall"] <- DTrecall31
MetricsProject2["user31", "DTf1score"] <- DTf1score31
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train31, type = "C-classification")
#test model
p <- predict(svmm, test31, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test31[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision31 <- TP/(TP+FP)
#Recall
SVMrecall31 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score31 <- 2*(SVMrecall31 * SVMprecision31) / (SVMrecall31 + SVMprecision31)
MetricsProject2["user31", "TP_s"] <- TP
MetricsProject2["user31", "TN_s"] <- TN
MetricsProject2["user31", "FP_s"] <- FP
MetricsProject2["user31", "FN_s"] <- FN
MetricsProject2["user31", "sprecision"] <- SVMprecision31
MetricsProject2["user31", "srecall"] <- SVMrecall31
MetricsProject2["user31", "sf1score"] <- SVMf1score31
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test32 <- read.csv("TrainingTestData/test32.csv")
train32 <- read.csv("TrainingTestData/training32.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train32, method = "class")
#test model
p <- predict(dtm, test32, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test32[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision32 <- TP/(TP+FP)
#Recall
DTrecall32 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score32 <- 2*(DTrecall32 * DTprecision32) / (DTrecall32 + DTprecision32)
MetricsProject2["user32", "TP_dt"] <- TP
MetricsProject2["user32", "TN_dt"] <- TN
MetricsProject2["user32", "FP_dt"] <- FP
MetricsProject2["user32", "FN_dt"] <- FN
MetricsProject2["user32", "DTprecision"] <- DTprecision32
MetricsProject2["user32", "DTrecall"] <- DTrecall32
MetricsProject2["user32", "DTf1score"] <- DTf1score32
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train32, type = "C-classification")
#test model
p <- predict(svmm, test32, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test32[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision32 <- TP/(TP+FP)
#Recall
SVMrecall32 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score32 <- 2*(SVMrecall32 * SVMprecision32) / (SVMrecall32 + SVMprecision32)
MetricsProject2["user32", "TP_s"] <- TP
MetricsProject2["user32", "TN_s"] <- TN
MetricsProject2["user32", "FP_s"] <- FP
MetricsProject2["user32", "FN_s"] <- FN
MetricsProject2["user32", "sprecision"] <- SVMprecision32
MetricsProject2["user32", "srecall"] <- SVMrecall32
MetricsProject2["user32", "sf1score"] <- SVMf1score32
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test33 <- read.csv("TrainingTestData/test33.csv")
train33 <- read.csv("TrainingTestData/training33.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train33, method = "class")
#test model
p <- predict(dtm, test33, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test33[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision33 <- TP/(TP+FP)
#Recall
DTrecall33 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score33 <- 2*(DTrecall33 * DTprecision33) / (DTrecall33 + DTprecision33)
MetricsProject2["user33", "TP_dt"] <- TP
MetricsProject2["user33", "TN_dt"] <- TN
MetricsProject2["user33", "FP_dt"] <- FP
MetricsProject2["user33", "FN_dt"] <- FN
MetricsProject2["user33", "DTprecision"] <- DTprecision33
MetricsProject2["user33", "DTrecall"] <- DTrecall33
MetricsProject2["user33", "DTf1score"] <- DTf1score33
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train33, type = "C-classification")
#test model
p <- predict(svmm, test33, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test33[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision33 <- TP/(TP+FP)
#Recall
SVMrecall33 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score33 <- 2*(SVMrecall33 * SVMprecision33) / (SVMrecall33 + SVMprecision33)
MetricsProject2["user33", "TP_s"] <- TP
MetricsProject2["user33", "TN_s"] <- TN
MetricsProject2["user33", "FP_s"] <- FP
MetricsProject2["user33", "FN_s"] <- FN
MetricsProject2["user33", "sprecision"] <- SVMprecision33
MetricsProject2["user33", "srecall"] <- SVMrecall33
MetricsProject2["user33", "sf1score"] <- SVMf1score33
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test34 <- read.csv("TrainingTestData/test34.csv")
train34 <- read.csv("TrainingTestData/training34.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train34, method = "class")
#test model
p <- predict(dtm, test34, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test34[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision34 <- TP/(TP+FP)
#Recall
DTrecall34 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score34 <- 2*(DTrecall34 * DTprecision34) / (DTrecall34 + DTprecision34)
MetricsProject2["user34", "TP_dt"] <- TP
MetricsProject2["user34", "TN_dt"] <- TN
MetricsProject2["user34", "FP_dt"] <- FP
MetricsProject2["user34", "FN_dt"] <- FN
MetricsProject2["user34", "DTprecision"] <- DTprecision34
MetricsProject2["user34", "DTrecall"] <- DTrecall34
MetricsProject2["user34", "DTf1score"] <- DTf1score34
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train34, type = "C-classification")
#test model
p <- predict(svmm, test34, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test34[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision34 <- TP/(TP+FP)
#Recall
SVMrecall34 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score34 <- 2*(SVMrecall34 * SVMprecision34) / (SVMrecall34 + SVMprecision34)
MetricsProject2["user34", "TP_s"] <- TP
MetricsProject2["user34", "TN_s"] <- TN
MetricsProject2["user34", "FP_s"] <- FP
MetricsProject2["user34", "FN_s"] <- FN
MetricsProject2["user34", "sprecision"] <- SVMprecision34
MetricsProject2["user34", "srecall"] <- SVMrecall34
MetricsProject2["user34", "sf1score"] <- SVMf1score34
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test36 <- read.csv("TrainingTestData/test36.csv")
train36 <- read.csv("TrainingTestData/training36.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train36, method = "class")
#test model
p <- predict(dtm, test36, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test36[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision36 <- TP/(TP+FP)
#Recall
DTrecall36 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score36 <- 2*(DTrecall36 * DTprecision36) / (DTrecall36 + DTprecision36)
MetricsProject2["user36", "TP_dt"] <- TP
MetricsProject2["user36", "TN_dt"] <- TN
MetricsProject2["user36", "FP_dt"] <- FP
MetricsProject2["user36", "FN_dt"] <- FN
MetricsProject2["user36", "DTprecision"] <- DTprecision36
MetricsProject2["user36", "DTrecall"] <- DTrecall36
MetricsProject2["user36", "DTf1score"] <- DTf1score36
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train36, type = "C-classification")
#test model
p <- predict(svmm, test36, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test36[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision36 <- TP/(TP+FP)
#Recall
SVMrecall36 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score36 <- 2*(SVMrecall36 * SVMprecision36) / (SVMrecall36 + SVMprecision36)
MetricsProject2["user36", "TP_s"] <- TP
MetricsProject2["user36", "TN_s"] <- TN
MetricsProject2["user36", "FP_s"] <- FP
MetricsProject2["user36", "FN_s"] <- FN
MetricsProject2["user36", "sprecision"] <- SVMprecision36
MetricsProject2["user36", "srecall"] <- SVMrecall36
MetricsProject2["user36", "sf1score"] <- SVMf1score36
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test37 <- read.csv("TrainingTestData/test37.csv")
train37 <- read.csv("TrainingTestData/training37.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train37, method = "class")
#test model
p <- predict(dtm, test37, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test37[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision37 <- TP/(TP+FP)
#Recall
DTrecall37 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score37 <- 2*(DTrecall37 * DTprecision37) / (DTrecall37 + DTprecision37)
MetricsProject2["user37", "TP_dt"] <- TP
MetricsProject2["user37", "TN_dt"] <- TN
MetricsProject2["user37", "FP_dt"] <- FP
MetricsProject2["user37", "FN_dt"] <- FN
MetricsProject2["user37", "DTprecision"] <- DTprecision37
MetricsProject2["user37", "DTrecall"] <- DTrecall37
MetricsProject2["user37", "DTf1score"] <- DTf1score37
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train37, type = "C-classification")
#test model
p <- predict(svmm, test37, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test37[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision37 <- TP/(TP+FP)
#Recall
SVMrecall37 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score37 <- 2*(SVMrecall37 * SVMprecision37) / (SVMrecall37 + SVMprecision37)
MetricsProject2["user37", "TP_s"] <- TP
MetricsProject2["user37", "TN_s"] <- TN
MetricsProject2["user37", "FP_s"] <- FP
MetricsProject2["user37", "FN_s"] <- FN
MetricsProject2["user37", "sprecision"] <- SVMprecision37
MetricsProject2["user37", "srecall"] <- SVMrecall37
MetricsProject2["user37", "sf1score"] <- SVMf1score37
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test38 <- read.csv("TrainingTestData/test38.csv")
train38 <- read.csv("TrainingTestData/training38.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train38, method = "class")
#test model
p <- predict(dtm, test38, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test38[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision38 <- TP/(TP+FP)
#Recall
DTrecall38 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score38 <- 2*(DTrecall38 * DTprecision38) / (DTrecall38 + DTprecision38)
MetricsProject2["user38", "TP_dt"] <- TP
MetricsProject2["user38", "TN_dt"] <- TN
MetricsProject2["user38", "FP_dt"] <- FP
MetricsProject2["user38", "FN_dt"] <- FN
MetricsProject2["user38", "DTprecision"] <- DTprecision38
MetricsProject2["user38", "DTrecall"] <- DTrecall38
MetricsProject2["user38", "DTf1score"] <- DTf1score38
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train38, type = "C-classification")
#test model
p <- predict(svmm, test38, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test38[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision38 <- TP/(TP+FP)
#Recall
SVMrecall38 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score38 <- 2*(SVMrecall38 * SVMprecision38) / (SVMrecall38 + SVMprecision38)
MetricsProject2["user38", "TP_s"] <- TP
MetricsProject2["user38", "TN_s"] <- TN
MetricsProject2["user38", "FP_s"] <- FP
MetricsProject2["user38", "FN_s"] <- FN
MetricsProject2["user38", "sprecision"] <- SVMprecision38
MetricsProject2["user38", "srecall"] <- SVMrecall38
MetricsProject2["user38", "sf1score"] <- SVMf1score38
write.csv(MetricsProject2, "MetricsProject2.csv")



###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test39 <- read.csv("TrainingTestData/test39.csv")
train39 <- read.csv("TrainingTestData/training39.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train39, method = "class")
#test model
p <- predict(dtm, test39, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test39[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision39 <- TP/(TP+FP)
#Recall
DTrecall39 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score39 <- 2*(DTrecall39 * DTprecision39) / (DTrecall39 + DTprecision39)
MetricsProject2["user39", "TP_dt"] <- TP
MetricsProject2["user39", "TN_dt"] <- TN
MetricsProject2["user39", "FP_dt"] <- FP
MetricsProject2["user39", "FN_dt"] <- FN
MetricsProject2["user39", "DTprecision"] <- DTprecision39
MetricsProject2["user39", "DTrecall"] <- DTrecall39
MetricsProject2["user39", "DTf1score"] <- DTf1score39
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train39, type = "C-classification")
#test model
p <- predict(svmm, test39, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test39[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision39 <- TP/(TP+FP)
#Recall
SVMrecall39 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score39 <- 2*(SVMrecall39 * SVMprecision39) / (SVMrecall39 + SVMprecision39)
MetricsProject2["user39", "TP_s"] <- TP
MetricsProject2["user39", "TN_s"] <- TN
MetricsProject2["user39", "FP_s"] <- FP
MetricsProject2["user39", "FN_s"] <- FN
MetricsProject2["user39", "sprecision"] <- SVMprecision39
MetricsProject2["user39", "srecall"] <- SVMrecall39
MetricsProject2["user39", "sf1score"] <- SVMf1score39
write.csv(MetricsProject2, "MetricsProject2.csv")




###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test40 <- read.csv("TrainingTestData/test40.csv")
train40 <- read.csv("TrainingTestData/training40.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train40, method = "class")
#test model
p <- predict(dtm, test40, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test40[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision40 <- TP/(TP+FP)
#Recall
DTrecall40 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score40 <- 2*(DTrecall40 * DTprecision40) / (DTrecall40 + DTprecision40)
MetricsProject2["user40", "TP_dt"] <- TP
MetricsProject2["user40", "TN_dt"] <- TN
MetricsProject2["user40", "FP_dt"] <- FP
MetricsProject2["user40", "FN_dt"] <- FN
MetricsProject2["user40", "DTprecision"] <- DTprecision40
MetricsProject2["user40", "DTrecall"] <- DTrecall40
MetricsProject2["user40", "DTf1score"] <- DTf1score40
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train40, type = "C-classification")
#test model
p <- predict(svmm, test40, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test40[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision40 <- TP/(TP+FP)
#Recall
SVMrecall40 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score40 <- 2*(SVMrecall40 * SVMprecision40) / (SVMrecall40 + SVMprecision40)
MetricsProject2["user40", "TP_s"] <- TP
MetricsProject2["user40", "TN_s"] <- TN
MetricsProject2["user40", "FP_s"] <- FP
MetricsProject2["user40", "FN_s"] <- FN
MetricsProject2["user40", "sprecision"] <- SVMprecision40
MetricsProject2["user40", "srecall"] <- SVMrecall40
MetricsProject2["user40", "sf1score"] <- SVMf1score40
write.csv(MetricsProject2, "MetricsProject2.csv")


###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
test41 <- read.csv("TrainingTestData/test41.csv")
train41 <- read.csv("TrainingTestData/training41.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., train41, method = "class")
#test model
p <- predict(dtm, test41, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test41[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecision41 <- TP/(TP+FP)
#Recall
DTrecall41 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1score41 <- 2*(DTrecall41 * DTprecision41) / (DTrecall41 + DTprecision41)
MetricsProject2["user41", "TP_dt"] <- TP
MetricsProject2["user41", "TN_dt"] <- TN
MetricsProject2["user41", "FP_dt"] <- FP
MetricsProject2["user41", "FN_dt"] <- FN
MetricsProject2["user41", "DTprecision"] <- DTprecision41
MetricsProject2["user41", "DTrecall"] <- DTrecall41
MetricsProject2["user41", "DTf1score"] <- DTf1score41
write.csv(MetricsProject2, "MetricsProject2.csv")


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., train41, type = "C-classification")
#test model
p <- predict(svmm, test41, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test41[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecision41 <- TP/(TP+FP)
#Recall
SVMrecall41 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1score41 <- 2*(SVMrecall41 * SVMprecision41) / (SVMrecall41 + SVMprecision41)
MetricsProject2["user41", "TP_s"] <- TP
MetricsProject2["user41", "TN_s"] <- TN
MetricsProject2["user41", "FP_s"] <- FP
MetricsProject2["user41", "FN_s"] <- FN
MetricsProject2["user41", "sprecision"] <- SVMprecision41
MetricsProject2["user41", "srecall"] <- SVMrecall41
MetricsProject2["user41", "sf1score"] <- SVMf1score41
write.csv(MetricsProject2, "MetricsProject2.csv")



###################################
##                               ##
##    PHASE 1: Neural Networks   ##
##                               ##
###################################


#Make dataframe to store result of matrices
MetricsProject2nn <- data.frame(matrix(0, 30, 7))
row.names(MetricsProject2nn) <- c("user09", "user10", "user11", "user12", "user13", "user14", "user16", "user17", "user18", "user19", "user21",
                                  "user22", "user23", "user24", "user25", "user26", "user27", "user28", "user29", "user30", "user31",
                                  "user32", "user33", "user34", "user36", "user37", "user38", "user39", "user40", "user41" )
colnames(MetricsProject2nn) <- c("TP_n", "TN_n", "FP_n", "FN_n", "nprecision", "nrecall", "nf1score")


NeuronFxn <- function(x){
  if (nrow(x) < 6000){
    hneurons <- 4
  } else if (nrow(x) < 10000){
    hneurons <- 5
  } else if (nrow(x) < 15000){
    hneurons <- 6
  } else if (nrow(x) < 20000){
    hneurons <- 7
  } else {
    hneurons <- 8
  }
  return(hneurons)
}


#Load installed package neuralnet
library("neuralnet")


#Create neural networks model
#train model
hneurons <- NeuronFxn(train09)
cn <- paste(colnames(train09)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train09, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test09[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test09[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision09 <- TP/(TP+FP)
#Recall
NNrecall09 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score09 <- 2*(NNrecall09 * NNprecision09) / (NNrecall09 + NNprecision09)
MetricsProject2nn["user09", "TP_n"] <- TP
MetricsProject2nn["user09", "TN_n"] <- TN
MetricsProject2nn["user09", "FP_n"] <- FP
MetricsProject2nn["user09", "FN_n"] <- FN
MetricsProject2nn["user09", "nprecision"] <- NNprecision09
MetricsProject2nn["user09", "nrecall"] <- NNrecall09
MetricsProject2nn["user09", "nf1score"] <- NNf1score09
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train10)
cn <- paste(colnames(train10)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train10, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test10[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test10[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision10 <- TP/(TP+FP)
#Recall
NNrecall10 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score10 <- 2*(NNrecall10 * NNprecision10) / (NNrecall10 + NNprecision10)
MetricsProject2nn["user10", "TP_n"] <- TP
MetricsProject2nn["user10", "TN_n"] <- TN
MetricsProject2nn["user10", "FP_n"] <- FP
MetricsProject2nn["user10", "FN_n"] <- FN
MetricsProject2nn["user10", "nprecision"] <- NNprecision10
MetricsProject2nn["user10", "nrecall"] <- NNrecall10
MetricsProject2nn["user10", "nf1score"] <- NNf1score10
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train11)
cn <- paste(colnames(train11)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train11, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test11[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test11[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision11 <- TP/(TP+FP)
#Recall
NNrecall11 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score11 <- 2*(NNrecall11 * NNprecision11) / (NNrecall11 + NNprecision11)
MetricsProject2nn["user11", "TP_n"] <- TP
MetricsProject2nn["user11", "TN_n"] <- TN
MetricsProject2nn["user11", "FP_n"] <- FP
MetricsProject2nn["user11", "FN_n"] <- FN
MetricsProject2nn["user11", "nprecision"] <- NNprecision11
MetricsProject2nn["user11", "nrecall"] <- NNrecall11
MetricsProject2nn["user11", "nf1score"] <- NNf1score11
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")





#Create neural networks model
#train model
hneurons <- NeuronFxn(train12)
cn <- paste(colnames(train12)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train12, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test12[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test12[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision12 <- TP/(TP+FP)
#Recall
NNrecall12 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score12 <- 2*(NNrecall12 * NNprecision12) / (NNrecall12 + NNprecision12)
MetricsProject2nn["user12", "TP_n"] <- TP
MetricsProject2nn["user12", "TN_n"] <- TN
MetricsProject2nn["user12", "FP_n"] <- FP
MetricsProject2nn["user12", "FN_n"] <- FN
MetricsProject2nn["user12", "nprecision"] <- NNprecision12
MetricsProject2nn["user12", "nrecall"] <- NNrecall12
MetricsProject2nn["user12", "nf1score"] <- NNf1score12
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train13)
cn <- paste(colnames(train13)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train13, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test13[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test13[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision13 <- TP/(TP+FP)
#Recall
NNrecall13 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score13 <- 2*(NNrecall13 * NNprecision13) / (NNrecall13 + NNprecision13)
MetricsProject2nn["user13", "TP_n"] <- TP
MetricsProject2nn["user13", "TN_n"] <- TN
MetricsProject2nn["user13", "FP_n"] <- FP
MetricsProject2nn["user13", "FN_n"] <- FN
MetricsProject2nn["user13", "nprecision"] <- NNprecision13
MetricsProject2nn["user13", "nrecall"] <- NNrecall13
MetricsProject2nn["user13", "nf1score"] <- NNf1score13
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train14)
cn <- paste(colnames(train14)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train14, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test14[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test14[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision14 <- TP/(TP+FP)
#Recall
NNrecall14 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score14 <- 2*(NNrecall14 * NNprecision14) / (NNrecall14 + NNprecision14)
MetricsProject2nn["user14", "TP_n"] <- TP
MetricsProject2nn["user14", "TN_n"] <- TN
MetricsProject2nn["user14", "FP_n"] <- FP
MetricsProject2nn["user14", "FN_n"] <- FN
MetricsProject2nn["user14", "nprecision"] <- NNprecision14
MetricsProject2nn["user14", "nrecall"] <- NNrecall14
MetricsProject2nn["user14", "nf1score"] <- NNf1score14
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train16)
cn <- paste(colnames(train16)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train16, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test16[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test16[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision16 <- TP/(TP+FP)
#Recall
NNrecall16 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score16 <- 2*(NNrecall16 * NNprecision16) / (NNrecall16 + NNprecision16)
MetricsProject2nn["user16", "TP_n"] <- TP
MetricsProject2nn["user16", "TN_n"] <- TN
MetricsProject2nn["user16", "FP_n"] <- FP
MetricsProject2nn["user16", "FN_n"] <- FN
MetricsProject2nn["user16", "nprecision"] <- NNprecision16
MetricsProject2nn["user16", "nrecall"] <- NNrecall16
MetricsProject2nn["user16", "nf1score"] <- NNf1score16
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train17)
cn <- paste(colnames(train17)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train17, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test17[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test17[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision17 <- TP/(TP+FP)
#Recall
NNrecall17 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score17 <- 2*(NNrecall17 * NNprecision17) / (NNrecall17 + NNprecision17)
MetricsProject2nn["user17", "TP_n"] <- TP
MetricsProject2nn["user17", "TN_n"] <- TN
MetricsProject2nn["user17", "FP_n"] <- FP
MetricsProject2nn["user17", "FN_n"] <- FN
MetricsProject2nn["user17", "nprecision"] <- NNprecision17
MetricsProject2nn["user17", "nrecall"] <- NNrecall17
MetricsProject2nn["user17", "nf1score"] <- NNf1score17
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train18)
cn <- paste(colnames(train18)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train18, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test18[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test18[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision18 <- TP/(TP+FP)
#Recall
NNrecall18 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score18 <- 2*(NNrecall18 * NNprecision18) / (NNrecall18 + NNprecision18)
MetricsProject2nn["user18", "TP_n"] <- TP
MetricsProject2nn["user18", "TN_n"] <- TN
MetricsProject2nn["user18", "FP_n"] <- FP
MetricsProject2nn["user18", "FN_n"] <- FN
MetricsProject2nn["user18", "nprecision"] <- NNprecision18
MetricsProject2nn["user18", "nrecall"] <- NNrecall18
MetricsProject2nn["user18", "nf1score"] <- NNf1score18
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train19)
cn <- paste(colnames(train19)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train19, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test19[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test19[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision19 <- TP/(TP+FP)
#Recall
NNrecall19 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score19 <- 2*(NNrecall19 * NNprecision19) / (NNrecall19 + NNprecision19)
MetricsProject2nn["user19", "TP_n"] <- TP
MetricsProject2nn["user19", "TN_n"] <- TN
MetricsProject2nn["user19", "FP_n"] <- FP
MetricsProject2nn["user19", "FN_n"] <- FN
MetricsProject2nn["user19", "nprecision"] <- NNprecision19
MetricsProject2nn["user19", "nrecall"] <- NNrecall19
MetricsProject2nn["user19", "nf1score"] <- NNf1score19
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train21)
cn <- paste(colnames(train21)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train21, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test21[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test21[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision21 <- TP/(TP+FP)
#Recall
NNrecall21 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score21 <- 2*(NNrecall21 * NNprecision21) / (NNrecall21 + NNprecision21)
MetricsProject2nn["user21", "TP_n"] <- TP
MetricsProject2nn["user21", "TN_n"] <- TN
MetricsProject2nn["user21", "FP_n"] <- FP
MetricsProject2nn["user21", "FN_n"] <- FN
MetricsProject2nn["user21", "nprecision"] <- NNprecision21
MetricsProject2nn["user21", "nrecall"] <- NNrecall21
MetricsProject2nn["user21", "nf1score"] <- NNf1score21
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train22)
cn <- paste(colnames(train22)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train22, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test22[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test22[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision22 <- TP/(TP+FP)
#Recall
NNrecall22 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score22 <- 2*(NNrecall22 * NNprecision22) / (NNrecall22 + NNprecision22)
MetricsProject2nn["user22", "TP_n"] <- TP
MetricsProject2nn["user22", "TN_n"] <- TN
MetricsProject2nn["user22", "FP_n"] <- FP
MetricsProject2nn["user22", "FN_n"] <- FN
MetricsProject2nn["user22", "nprecision"] <- NNprecision22
MetricsProject2nn["user22", "nrecall"] <- NNrecall22
MetricsProject2nn["user22", "nf1score"] <- NNf1score22
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train23)
cn <- paste(colnames(train23)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train23, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test23[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test23[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision23 <- TP/(TP+FP)
#Recall
NNrecall23 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score23 <- 2*(NNrecall23 * NNprecision23) / (NNrecall23 + NNprecision23)
MetricsProject2nn["user23", "TP_n"] <- TP
MetricsProject2nn["user23", "TN_n"] <- TN
MetricsProject2nn["user23", "FP_n"] <- FP
MetricsProject2nn["user23", "FN_n"] <- FN
MetricsProject2nn["user23", "nprecision"] <- NNprecision23
MetricsProject2nn["user23", "nrecall"] <- NNrecall23
MetricsProject2nn["user23", "nf1score"] <- NNf1score23
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train24)
cn <- paste(colnames(train24)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train24, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test24[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test24[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision24 <- TP/(TP+FP)
#Recall
NNrecall24 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score24 <- 2*(NNrecall24 * NNprecision24) / (NNrecall24 + NNprecision24)
MetricsProject2nn["user24", "TP_n"] <- TP
MetricsProject2nn["user24", "TN_n"] <- TN
MetricsProject2nn["user24", "FP_n"] <- FP
MetricsProject2nn["user24", "FN_n"] <- FN
MetricsProject2nn["user24", "nprecision"] <- NNprecision24
MetricsProject2nn["user24", "nrecall"] <- NNrecall24
MetricsProject2nn["user24", "nf1score"] <- NNf1score24
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train25)
cn <- paste(colnames(train25)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train25, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test25[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test25[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision25 <- TP/(TP+FP)
#Recall
NNrecall25 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score25 <- 2*(NNrecall25 * NNprecision25) / (NNrecall25 + NNprecision25)
MetricsProject2nn["user25", "TP_n"] <- TP
MetricsProject2nn["user25", "TN_n"] <- TN
MetricsProject2nn["user25", "FP_n"] <- FP
MetricsProject2nn["user25", "FN_n"] <- FN
MetricsProject2nn["user25", "nprecision"] <- NNprecision25
MetricsProject2nn["user25", "nrecall"] <- NNrecall25
MetricsProject2nn["user25", "nf1score"] <- NNf1score25
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train26)
cn <- paste(colnames(train26)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train26, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test26[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test26[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision26 <- TP/(TP+FP)
#Recall
NNrecall26 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score26 <- 2*(NNrecall26 * NNprecision26) / (NNrecall26 + NNprecision26)
MetricsProject2nn["user26", "TP_n"] <- TP
MetricsProject2nn["user26", "TN_n"] <- TN
MetricsProject2nn["user26", "FP_n"] <- FP
MetricsProject2nn["user26", "FN_n"] <- FN
MetricsProject2nn["user26", "nprecision"] <- NNprecision26
MetricsProject2nn["user26", "nrecall"] <- NNrecall26
MetricsProject2nn["user26", "nf1score"] <- NNf1score26
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")


#Create neural networks model
#train model
hneurons <- NeuronFxn(train27)
cn <- paste(colnames(train27)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train27, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test27[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test27[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision27 <- TP/(TP+FP)
#Recall
NNrecall27 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score27 <- 2*(NNrecall27 * NNprecision27) / (NNrecall27 + NNprecision27)
MetricsProject2nn["user27", "TP_n"] <- TP
MetricsProject2nn["user27", "TN_n"] <- TN
MetricsProject2nn["user27", "FP_n"] <- FP
MetricsProject2nn["user27", "FN_n"] <- FN
MetricsProject2nn["user27", "nprecision"] <- NNprecision27
MetricsProject2nn["user27", "nrecall"] <- NNrecall27
MetricsProject2nn["user27", "nf1score"] <- NNf1score27
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train28)
cn <- paste(colnames(train28)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train28, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test28[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test28[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision28 <- TP/(TP+FP)
#Recall
NNrecall28 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score28 <- 2*(NNrecall28 * NNprecision28) / (NNrecall28 + NNprecision28)
MetricsProject2nn["user28", "TP_n"] <- TP
MetricsProject2nn["user28", "TN_n"] <- TN
MetricsProject2nn["user28", "FP_n"] <- FP
MetricsProject2nn["user28", "FN_n"] <- FN
MetricsProject2nn["user28", "nprecision"] <- NNprecision28
MetricsProject2nn["user28", "nrecall"] <- NNrecall28
MetricsProject2nn["user28", "nf1score"] <- NNf1score28
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train29)
cn <- paste(colnames(train29)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train29,hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test29[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test29[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision29 <- TP/(TP+FP)
#Recall
NNrecall29 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score29 <- 2*(NNrecall29 * NNprecision29) / (NNrecall29 + NNprecision29)
MetricsProject2nn["user29", "TP_n"] <- TP
MetricsProject2nn["user29", "TN_n"] <- TN
MetricsProject2nn["user29", "FP_n"] <- FP
MetricsProject2nn["user29", "FN_n"] <- FN
MetricsProject2nn["user29", "nprecision"] <- NNprecision29
MetricsProject2nn["user29", "nrecall"] <- NNrecall29
MetricsProject2nn["user29", "nf1score"] <- NNf1score29
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train30)
cn <- paste(colnames(train30)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train30, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test30[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test30[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision30 <- TP/(TP+FP)
#Recall
NNrecall30 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score30 <- 2*(NNrecall30 * NNprecision30) / (NNrecall30 + NNprecision30)
MetricsProject2nn["user30", "TP_n"] <- TP
MetricsProject2nn["user30", "TN_n"] <- TN
MetricsProject2nn["user30", "FP_n"] <- FP
MetricsProject2nn["user30", "FN_n"] <- FN
MetricsProject2nn["user30", "nprecision"] <- NNprecision30
MetricsProject2nn["user30", "nrecall"] <- NNrecall30
MetricsProject2nn["user30", "nf1score"] <- NNf1score30
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train31)
cn <- paste(colnames(train31)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train31, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test31[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test31[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision31 <- TP/(TP+FP)
#Recall
NNrecall31 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score31 <- 2*(NNrecall31 * NNprecision31) / (NNrecall31 + NNprecision31)
MetricsProject2nn["user31", "TP_n"] <- TP
MetricsProject2nn["user31", "TN_n"] <- TN
MetricsProject2nn["user31", "FP_n"] <- FP
MetricsProject2nn["user31", "FN_n"] <- FN
MetricsProject2nn["user31", "nprecision"] <- NNprecision31
MetricsProject2nn["user31", "nrecall"] <- NNrecall31
MetricsProject2nn["user31", "nf1score"] <- NNf1score31
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train32)
cn <- paste(colnames(train32)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train32, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test32[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test32[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision32 <- TP/(TP+FP)
#Recall
NNrecall32 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score32 <- 2*(NNrecall32 * NNprecision32) / (NNrecall32 + NNprecision32)
MetricsProject2nn["user32", "TP_n"] <- TP
MetricsProject2nn["user32", "TN_n"] <- TN
MetricsProject2nn["user32", "FP_n"] <- FP
MetricsProject2nn["user32", "FN_n"] <- FN
MetricsProject2nn["user32", "nprecision"] <- NNprecision32
MetricsProject2nn["user32", "nrecall"] <- NNrecall32
MetricsProject2nn["user32", "nf1score"] <- NNf1score32
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train33)
cn <- paste(colnames(train33)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train33, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test33[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test33[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision33 <- TP/(TP+FP)
#Recall
NNrecall33 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score33 <- 2*(NNrecall33 * NNprecision33) / (NNrecall33 + NNprecision33)
MetricsProject2nn["user33", "TP_n"] <- TP
MetricsProject2nn["user33", "TN_n"] <- TN
MetricsProject2nn["user33", "FP_n"] <- FP
MetricsProject2nn["user33", "FN_n"] <- FN
MetricsProject2nn["user33", "nprecision"] <- NNprecision33
MetricsProject2nn["user33", "nrecall"] <- NNrecall33
MetricsProject2nn["user33", "nf1score"] <- NNf1score33
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train34)
cn <- paste(colnames(train34)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train34, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test34[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test34[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision34 <- TP/(TP+FP)
#Recall
NNrecall34 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score34 <- 2*(NNrecall34 * NNprecision34) / (NNrecall34 + NNprecision34)
MetricsProject2nn["user34", "TP_n"] <- TP
MetricsProject2nn["user34", "TN_n"] <- TN
MetricsProject2nn["user34", "FP_n"] <- FP
MetricsProject2nn["user34", "FN_n"] <- FN
MetricsProject2nn["user34", "nprecision"] <- NNprecision34
MetricsProject2nn["user34", "nrecall"] <- NNrecall34
MetricsProject2nn["user34", "nf1score"] <- NNf1score34
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train36)
cn <- paste(colnames(train36)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train36, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test36[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test36[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision36 <- TP/(TP+FP)
#Recall
NNrecall36 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score36 <- 2*(NNrecall36 * NNprecision36) / (NNrecall36 + NNprecision36)
MetricsProject2nn["user36", "TP_n"] <- TP
MetricsProject2nn["user36", "TN_n"] <- TN
MetricsProject2nn["user36", "FP_n"] <- FP
MetricsProject2nn["user36", "FN_n"] <- FN
MetricsProject2nn["user36", "nprecision"] <- NNprecision36
MetricsProject2nn["user36", "nrecall"] <- NNrecall36
MetricsProject2nn["user36", "nf1score"] <- NNf1score36
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#Create neural networks model
#train model
hneurons <- NeuronFxn(train37)
cn <- paste(colnames(train37)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train37, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test37[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test37[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision37 <- TP/(TP+FP)
#Recall
NNrecall37 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score37 <- 2*(NNrecall37 * NNprecision37) / (NNrecall37 + NNprecision37)
MetricsProject2nn["user37", "TP_n"] <- TP
MetricsProject2nn["user37", "TN_n"] <- TN
MetricsProject2nn["user37", "FP_n"] <- FP
MetricsProject2nn["user37", "FN_n"] <- FN
MetricsProject2nn["user37", "nprecision"] <- NNprecision37
MetricsProject2nn["user37", "nrecall"] <- NNrecall37
MetricsProject2nn["user37", "nf1score"] <- NNf1score37
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train38)
cn <- paste(colnames(train38)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train38,hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test38[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test38[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision38 <- TP/(TP+FP)
#Recall
NNrecall38 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score38 <- 2*(NNrecall38 * NNprecision38) / (NNrecall38 + NNprecision38)
MetricsProject2nn["user38", "TP_n"] <- TP
MetricsProject2nn["user38", "TN_n"] <- TN
MetricsProject2nn["user38", "FP_n"] <- FP
MetricsProject2nn["user38", "FN_n"] <- FN
MetricsProject2nn["user38", "nprecision"] <- NNprecision38
MetricsProject2nn["user38", "nrecall"] <- NNrecall38
MetricsProject2nn["user38", "nf1score"] <- NNf1score38
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train39)
cn <- paste(colnames(train39)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train39, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test39[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test39[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision39 <- TP/(TP+FP)
#Recall
NNrecall39 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score39 <- 2*(NNrecall39 * NNprecision39) / (NNrecall39 + NNprecision39)
MetricsProject2nn["user39", "TP_n"] <- TP
MetricsProject2nn["user39", "TN_n"] <- TN
MetricsProject2nn["user39", "FP_n"] <- FP
MetricsProject2nn["user39", "FN_n"] <- FN
MetricsProject2nn["user39", "nprecision"] <- NNprecision39
MetricsProject2nn["user39", "nrecall"] <- NNrecall39
MetricsProject2nn["user39", "nf1score"] <- NNf1score39
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train40)
cn <- paste(colnames(train40)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train40, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test40[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test40[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision40 <- TP/(TP+FP)
#Recall
NNrecall40 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score40 <- 2*(NNrecall40 * NNprecision40) / (NNrecall40 + NNprecision40)
MetricsProject2nn["user40", "TP_n"] <- TP
MetricsProject2nn["user40", "TN_n"] <- TN
MetricsProject2nn["user40", "FP_n"] <- FP
MetricsProject2nn["user40", "FN_n"] <- FN
MetricsProject2nn["user40", "nprecision"] <- NNprecision40
MetricsProject2nn["user40", "nrecall"] <- NNrecall40
MetricsProject2nn["user40", "nf1score"] <- NNf1score40
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")



#Create neural networks model
#train model
hneurons <- NeuronFxn(train41)
cn <- paste(colnames(train41)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = train41, hidden=hneurons, linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, test41[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(test41[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecision41 <- TP/(TP+FP)
#Recall
NNrecall41 <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1score41 <- 2*(NNrecall41 * NNprecision41) / (NNrecall41 + NNprecision41)
MetricsProject2nn["user41", "TP_n"] <- TP
MetricsProject2nn["user41", "TN_n"] <- TN
MetricsProject2nn["user41", "FP_n"] <- FP
MetricsProject2nn["user41", "FN_n"] <- FN
MetricsProject2nn["user41", "nprecision"] <- NNprecision41
MetricsProject2nn["user41", "nrecall"] <- NNrecall41
MetricsProject2nn["user41", "nf1score"] <- NNf1score41
write.csv(MetricsProject2nn, "MetricsProject2nn.csv")




#######################################
##                                   ##
##    PHASE 1: Accuracy metrics      ##
##                                   ##
#######################################

dtsvm_users <- read.csv("MetricsProject2.csv")
nn_users <- read.csv("MetricsProject2nn.csv")
AccuracyMetricsPhase1 <- cbind(dtsvm_users[,1], dtsvm_users[,6:8], dtsvm_users[,13:15], nn_users[,6:8])
names(AccuracyMetricsPhase1) <- (c("User", "DT Precision", "DT Recall", "DT F1 Score", "SVM Precision", "SVM Recall", "SVM F1 Score", "NN Precision", "NN Recall", "NN F1 Score"))
write.csv(AccuracyMetricsPhase1, "AccuracyMetricsPhase1.csv", row.names = FALSE)




#####################################
##                                 ##
##    PHASE 2: TrainingTestData    ##
##                                 ##
#####################################


# Combine the user features for PCA into four categories - eating test, eating training, noneating test and noneating training
training_eating_AllUsers <- rbind(eating09, eating10, eating11, eating12, eating13, eating14, eating16, eating17, eating18, 
                                  eating19, eating21, eating22, eating23, eating24, eating25, eating26, eating27, eating28)
test_eating_AllUsers <- rbind(eating29, eating30, eating31, eating32, eating33, eating34, eating36, eating37, eating38, eating39, eating40, eating41)

training_non_eating_AllUsers <- rbind(non_eating09, non_eating10, non_eating11, non_eating12, non_eating13, non_eating14, non_eating16, non_eating17, non_eating18, 
                                      non_eating19, non_eating21, non_eating22, non_eating23, non_eating24, non_eating25, non_eating26, non_eating27, non_eating28)
test_non_eating_AllUsers <- rbind(non_eating29, non_eating30, non_eating31, non_eating32, non_eating33, non_eating34, non_eating36, non_eating37, non_eating38, non_eating39, non_eating40, non_eating41)

#label the first rows, 1 for eating test,  3 for eating training, 0 for noneating test and 2 for noneating training
training_eating_AllUsers$V1 <- 3
test_eating_AllUsers$V1 <-  1
training_non_eating_AllUsers$V1 <- 2
test_non_eating_AllUsers$V1 <- 0

#Generate feature arrays
pcaInput <- rbind(test_eating_AllUsers, test_non_eating_AllUsers, training_eating_AllUsers, training_non_eating_AllUsers)
pcaInput_clean <- pcaInput[c(1:6)]
pcaInput_clean[c(2:6)] <- pcaInput[c(10:14)]
pcaInput <- pcaInput_clean[c(2:6)] 
names(pcaInput) <- (c("Mean", "Median", "STD", "Max", "ICA"))
# do pca
eat_pca <- princomp(pcaInput, cor = TRUE, scores = TRUE)
#Summary information of the pca to know contribution of each component and to determine which PCs to use
summary(eat_pca)
#Generate new features
newFeatures <- eat_pca$scores
#Generate new features matrix
newFeatureMatrix <- eat_pca$scores[,1:3] %*% t(eat_pca$loadings[,1:3])
newFeatureMatrix <- scale(newFeatureMatrix, center = -eat_pca$center, scale = TRUE)
pcaClean <- data.frame("Eat1Noneat0"=pcaInput_clean$V1, newFeatureMatrix)
#Subset based on value of first Column to retrieve testing and traing set.
pcaClean_eating_test <- pcaClean[pcaClean$Eat1Noneat0 == 1,]
pcaClean_non_eating_test <- pcaClean[pcaClean$Eat1Noneat0 == 0,]
pcaClean_eating_training <- pcaClean[pcaClean$Eat1Noneat0 == 3,]
pcaClean_non_eating_training <- pcaClean[pcaClean$Eat1Noneat0 == 2,]
#Relabel first column of training subsets
pcaClean_eating_training$Eat1Noneat0 <- 1
pcaClean_non_eating_training$Eat1Noneat0 <- 0
#Join subsets to get training and testing data
trainingdata <- rbind(pcaClean_eating_training, pcaClean_non_eating_training)
testdata <- rbind(pcaClean_eating_test, pcaClean_non_eating_test)
write.table(trainingdata, file = "trainingAll.csv", sep = ",", row.names=FALSE)
write.table(testdata, file = "testAll.csv", sep = ",", row.names=FALSE)



#######################################
##                                   ##
##    PHASE 2: Decision Tree & SVM   ##
##                                   ##
#######################################

###### Decision Tree ######
###########################
#Get previously gnearated training and testing data
testAll <- read.csv("TrainingTestData/testAll.csv")
trainAll <- read.csv("TrainingTestData/trainingAll.csv")
#Create decision tree model
#train model
dtm <- rpart(Eat1Noneat0~., trainAll, method = "class")
#test model
p <- predict(dtm, testAll, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(testAll[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
DTprecisionAll <- TP/(TP+FP)
#Recall
DTrecallAll <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
DTf1scoreAll <- 2*(DTrecallAll * DTprecisionAll) / (DTrecallAll + DTprecisionAll)


###### SVM ######
#################
#Create SVM model
#train model
svmm <- svm(Eat1Noneat0~., trainAll, type = "C-classification")
#test model
p <- predict(svmm, testAll, type="class")
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(testAll[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
SVMprecisionAll <- TP/(TP+FP)
#Recall
SVMrecallAll <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
SVMf1scoreAll <- 2*(SVMrecallAll * SVMprecisionAll) / (SVMrecallAll + SVMprecisionAll)



###################################
##                               ##
##    PHASE 2: Neural Networks   ##
##                               ##
###################################

#Because of the large size of trainAll,  a subset was used for training.
# To get the subset, emg data from Phase 1 of Project1 was used.
#Got the first 2000 rows of eating and non-eating csvs which are the emg data that I generated in Project1.
eating09 <- read.csv("AllCSV_emg/eating09.csv", header = FALSE)[1:1000, ]
non_eating09 <- read.csv("AllCSV_emg/non_eating09.csv", header = FALSE)[1:1000, ]
eating10 <- read.csv("AllCSV_emg/eating10.csv", header = FALSE)[1:1000, ]
non_eating10 <- read.csv("AllCSV_emg/non_eating10.csv", header = FALSE)[1:1000, ]
eating11 <- read.csv("AllCSV_emg/eating11.csv", header = FALSE)[1:1000, ]
non_eating11 <- read.csv("AllCSV_emg/non_eating11.csv", header = FALSE)[1:1000, ]
eating12 <- read.csv("AllCSV_emg/eating12.csv", header = FALSE)[1:1000, ]
non_eating12 <- read.csv("AllCSV_emg/non_eating12.csv", header = FALSE)[1:1000, ]
eating13 <- read.csv("AllCSV_emg/eating13.csv", header = FALSE)[1:1000, ]
non_eating13 <- read.csv("AllCSV_emg/non_eating13.csv", header = FALSE)[1:1000, ]
eating14 <- read.csv("AllCSV_emg/eating14.csv", header = FALSE)[1:1000, ]
non_eating14 <- read.csv("AllCSV_emg/non_eating14.csv", header = FALSE)[1:1000, ]
eating16 <- read.csv("AllCSV_emg/eating16.csv", header = FALSE)[1:1000, ]
non_eating16 <- read.csv("AllCSV_emg/non_eating16.csv", header = FALSE)[1:1000, ]
eating17 <- read.csv("AllCSV_emg/eating17.csv", header = FALSE)[1:1000, ]
non_eating17 <- read.csv("AllCSV_emg/non_eating17.csv", header = FALSE)[1:1000, ]
eating18 <- read.csv("AllCSV_emg/eating18.csv", header = FALSE)[1:1000, ]
non_eating18 <- read.csv("AllCSV_emg/non_eating18.csv", header = FALSE)[1:1000, ]
eating19 <- read.csv("AllCSV_emg/eating19.csv", header = FALSE)[1:1000, ]
non_eating19 <- read.csv("AllCSV_emg/non_eating19.csv", header = FALSE)[1:1000, ]
eating21 <- read.csv("AllCSV_emg/eating21.csv", header = FALSE)[1:1000, ]
non_eating21 <- read.csv("AllCSV_emg/non_eating21.csv", header = FALSE)[1:1000, ]
eating22 <- read.csv("AllCSV_emg/eating22.csv", header = FALSE)[1:1000, ]
non_eating22 <- read.csv("AllCSV_emg/non_eating22.csv", header = FALSE)[1:1000, ]
eating23 <- read.csv("AllCSV_emg/eating23.csv", header = FALSE)[1:1000, ]
non_eating23 <- read.csv("AllCSV_emg/non_eating23.csv", header = FALSE)[1:1000, ]
eating24 <- read.csv("AllCSV_emg/eating24.csv", header = FALSE)[1:1000, ]
non_eating24 <- read.csv("AllCSV_emg/non_eating24.csv", header = FALSE)[1:1000, ]
eating25 <- read.csv("AllCSV_emg/eating25.csv", header = FALSE)[1:1000, ]
non_eating25 <- read.csv("AllCSV_emg/non_eating25.csv", header = FALSE)[1:1000, ]
eating26 <- read.csv("AllCSV_emg/eating26.csv", header = FALSE)[1:1000, ]
non_eating26 <- read.csv("AllCSV_emg/non_eating26.csv", header = FALSE)[1:1000, ]
eating27 <- read.csv("AllCSV_emg/eating27.csv", header = FALSE)[1:1000, ]
non_eating27 <- read.csv("AllCSV_emg/non_eating27.csv", header = FALSE)[1:1000, ]
eating28 <- read.csv("AllCSV_emg/eating28.csv", header = FALSE)[1:1000, ]
non_eating28 <- read.csv("AllCSV_emg/non_eating28.csv", header = FALSE)[1:1000, ]
eating29 <- read.csv("AllCSV_emg/eating29.csv", header = FALSE)[1:1000, ]
non_eating29 <- read.csv("AllCSV_emg/non_eating29.csv", header = FALSE)[1:1000, ]
eating30 <- read.csv("AllCSV_emg/eating30.csv", header = FALSE)[1:1000, ]
non_eating30 <- read.csv("AllCSV_emg/non_eating30.csv", header = FALSE)[1:1000, ]
eating31 <- read.csv("AllCSV_emg/eating31.csv", header = FALSE)[1:1000, ]
non_eating31 <- read.csv("AllCSV_emg/non_eating31.csv", header = FALSE)[1:1000, ]
eating32 <- read.csv("AllCSV_emg/eating32.csv", header = FALSE)[1:1000, ]
non_eating32 <- read.csv("AllCSV_emg/non_eating32.csv", header = FALSE)[1:1000, ]
eating33 <- read.csv("AllCSV_emg/eating33.csv", header = FALSE)[1:1000, ]
non_eating33 <- read.csv("AllCSV_emg/non_eating33.csv", header = FALSE)[1:1000, ]
eating34 <- read.csv("AllCSV_emg/eating34.csv", header = FALSE)[1:1000, ]
non_eating34 <- read.csv("AllCSV_emg/non_eating34.csv", header = FALSE)[1:1000, ]
eating36 <- read.csv("AllCSV_emg/eating36.csv", header = FALSE)[1:1000, ]
non_eating36 <- read.csv("AllCSV_emg/non_eating36.csv", header = FALSE)[1:1000, ]
eating37 <- read.csv("AllCSV_emg/eating37.csv", header = FALSE)[1:1000, ]
non_eating37 <- read.csv("AllCSV_emg/non_eating37.csv", header = FALSE)[1:1000, ]
eating38 <- read.csv("AllCSV_emg/eating38.csv", header = FALSE)[1:1000, ]
non_eating38 <- read.csv("AllCSV_emg/non_eating38.csv", header = FALSE)[1:1000, ]
eating39 <- read.csv("AllCSV_emg/eating39.csv", header = FALSE)[1:1000, ]
non_eating39 <- read.csv("AllCSV_emg/non_eating39.csv", header = FALSE)[1:1000, ]
eating40 <- read.csv("AllCSV_emg/eating40.csv", header = FALSE)[1:1000, ]
non_eating40 <- read.csv("AllCSV_emg/non_eating40.csv", header = FALSE)[1:1000, ]
eating41 <- read.csv("AllCSV_emg/eating41.csv", header = FALSE)[1:1000, ]
non_eating41 <- read.csv("AllCSV_emg/non_eating41.csv", header = FALSE)[1:1000, ]

#Joined the first 18 eating data followed by the first 18 non-eating data followed by the remaining 12 eating and lastly 12 non-eating data
#This way after feature extraction and PCA, I will just slice out the top 72,000 rows (which will consist of the first 18 users' data) for training and 
#the remaining 12 users (48,000 rows) will be for testing
extraction_input_All <- rbind(eating09, eating10, eating11, eating12, eating13, eating14, eating16, eating17, eating18, 
                              eating19, eating21, eating22, eating23, eating24, eating25, eating26,eating27, eating28,
                              non_eating09, non_eating10, non_eating11, non_eating12, non_eating13, non_eating14, non_eating16, non_eating17, non_eating18, 
                              non_eating19, non_eating21, non_eating22, non_eating23, non_eating24, non_eating25, non_eating26, non_eating27, non_eating28,
                              eating29, eating30, eating31, eating32, eating33, eating34, eating36, eating37, eating38, eating39, eating40, eating41,
                              non_eating29, non_eating30, non_eating31, non_eating32, non_eating33, non_eating34, non_eating36, non_eating37, non_eating38, 
                              non_eating39, non_eating40, non_eating41)


##### FEATURE EXTRACTION #####

# Compute Mean of rows and adjoin to datasets
extraction_input_All$V10 <- rowMeans(extraction_input_All[c(2:9)])

# Compute Median of rows and adjoin to datasets
# load installed package matrixStats
library(matrixStats)
extraction_input_All$V11 <- rowMedians(as.matrix(extraction_input_All[c(2:9)]))

# Compute std of rows and adjoin to datasets
extraction_input_All$V12 <- rowSds(as.matrix(extraction_input_All[c(2:9)]))

# Compute Max of rows and adjoin to datasets
extraction_input_All$V13 <- rowMaxs(as.matrix(extraction_input_All[c(2:9)]))

#Compute ICA
#Load installed package fastICA
library(fastICA)
# For ICA, I am using the reduced feature from mean
# seperate out the eatng and non-eating data
#Eating the first 18 * 2000 rows (eating of training down the road) and the first 12 * 2000 rows 
#(after the second 18 * 2000 rows, which is non-eating for training down the road)
eatAll <- c(extraction_input_All$V10[1 : 18000], extraction_input_All$V10[36001 : 48000])
# the noneating
non_eatAll <- c(extraction_input_All$V10[18001 : 36000], extraction_input_All$V10[48001 : 60000])
# Combine eat and non-eat into vector variable S
S <- cbind(eatAll, non_eatAll)
# Make mixing matrix
A <- matrix(rnorm(4), 2, 2)
# Mix two signals
X <- as.matrix(S) %*% A
# ICA for extracting independent sources from mixed signals
a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
#Rejoin the ICA extracted features back in the other it was in extraction_input_All
# and adjoin ICA extracted features to datasets
extraction_input_All$V14  <- c(a$S[1:18000,1], a$S[1:18000,2], a$S[18001:30000,1], a$S[18001:30000,2])

#Generate feature arrays
pcaInput_clean <- extraction_input_All[c(1:6)]
pcaInput_clean[c(2:6)] <- extraction_input_All[c(10:14)]
pcaInput <- pcaInput_clean[c(2:6)] 
names(pcaInput) <- (c("Mean", "Median", "STD", "Max", "ICA"))
# do pca
eat_pca <- princomp(pcaInput, cor = TRUE, scores = TRUE)
#Summary information of the pca to know contribution of each component and to determine which PCs to use
summary(eat_pca)
#Generate new features
newFeatures <- eat_pca$scores
#Generate new features matrix
newFeatureMatrix <- eat_pca$scores[,1:3] %*% t(eat_pca$loadings[,1:3])
newFeatureMatrix <- scale(newFeatureMatrix, center = -eat_pca$center, scale = TRUE)
pcaClean <- data.frame("Eat1Noneat0"=pcaInput_clean$V1, newFeatureMatrix)

# Replace "eating" with 1 and "non-eating" with 0 in first column
pcaClean$Eat1Noneat0 <- gsub('non-eating', 0, pcaClean$Eat1Noneat0)
pcaClean$Eat1Noneat0 <- gsub('eating', 1, pcaClean$Eat1Noneat0)

#Subset based on prior knowledge on which rows will be for training/testing.
trainingdata <- pcaClean[1:36000, ]
testdata <- pcaClean[36001:60000, ]
write.table(trainingdata, file = "nntrainingAll.csv", sep = ",", row.names=FALSE)
write.table(testdata, file = "nntestAll.csv", sep = ",", row.names=FALSE)

###### Neural Networks ######
#############################
#Get previously generated training and testing data
testAll <- read.csv("nntestAll.csv")
trainAll <- read.csv("nntrainingAll.csv")
#Create neural networks model
#train model
cn <- paste(colnames(trainAll)[2:6], collapse = ' + ')
fo <- as.formula(paste('Eat1Noneat0', '~', cn)) 
set.seed(1)
nnm <- neuralnet(fo, data = trainAll, hidden=c(3,5), linear.output=FALSE, threshold = 0.1)
#test model
p <- round(compute(nnm, testAll[,-1])$net.result)
#Get a confusion matrix with observation as row and prediction as column
confmat <- table(testAll[,1], p)
# Get true positive, true negative, false positive and false negative
# Note that eating is labelled 1 and noneating 0
TP <- confmat["1","1"]
TN <- confmat["0","0"]
FP <- confmat["0","1"]
FN <- confmat["1","0"]
#Precision 
NNprecisionAll <- TP/(TP+FP)
#Recall
NNrecallAll <- TP/(TP+FN)
#F1 Score = 2*(Recall * Precision) / (Recall + Precision)
NNf1scoreAll <- 2*(NNrecallAll * NNprecisionAll) / (NNrecallAll + NNprecisionAll)


#######################################
##                                   ##
##    PHASE 2: Accuracy metrics      ##
##                                   ##
#######################################

AccuracyMetricsPhase2 <- data.frame(matrix(0, 3, 4))
names(AccuracyMetricsPhase2) <- c("ML Algorithms", "Precision", "Recall", "F1 Score")
AccuracyMetricsPhase2[1, 1] <- "Decision Trees"
AccuracyMetricsPhase2[1, 2] <- DTprecisionAll
AccuracyMetricsPhase2[1, 3] <- DTrecallAll
AccuracyMetricsPhase2[1, 4] <- DTf1scoreAll
AccuracyMetricsPhase2[2, 1] <- "Support Vector Machines"
AccuracyMetricsPhase2[2, 2] <- SVMprecisionAll
AccuracyMetricsPhase2[2, 3] <- SVMrecallAll
AccuracyMetricsPhase2[2, 4] <- SVMf1scoreAll
AccuracyMetricsPhase2[3, 1] <- "Neural networks"
AccuracyMetricsPhase2[3, 2] <- NNprecisionAll
AccuracyMetricsPhase2[3, 3] <- NNrecallAll
AccuracyMetricsPhase2[3, 4] <- NNf1scoreAll
write.csv(AccuracyMetricsPhase2, "AccuracyMetricsPhase2.csv", row.names = FALSE)


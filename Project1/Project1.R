
##################
##              ##
##    PHASE 1   ##
##              ##
##################

#Load installed package dplyr
library(dplyr)

# Set working directory 
setwd("~/ASU_MCS/CSE572/Project1/Data_Mining_Assign1Data")

# To generate row numbers of start of eat and end of eat for fork
user09_fork_gt <- read.csv("groundTruth/user9/fork/1503510449718.txt", header = FALSE)[c(1:2)]
head(user09_fork_gt)
user09_fork_gt$V3 = round(user09_fork_gt$V1*100/30)
user09_fork_gt$V4 = round(user09_fork_gt$V2*100/30)
user09_fork_gt <- user09_fork_gt[3:4]
head(user09_fork_gt)

# To generate row numbers of start of eat and end of eat for spoon
user09_spoon_gt <- read.csv("groundTruth/user9/spoon/1503510957301.txt", header = FALSE)[c(1:2)]
head(user09_spoon_gt)
user09_spoon_gt$V3 = round(user09_spoon_gt$V1*100/30)
user09_spoon_gt$V4 = round(user09_spoon_gt$V2*100/30)
user09_spoon_gt <- user09_spoon_gt[3:4]
head(user09_spoon_gt)


# Import EMG Files
user09_EMG_fork <- read.csv("MyoData/user09/fork/1503510449718_EMG.txt", header = FALSE)
user09_EMG_spoon <- read.csv("MyoData/user09/spoon/1503510957301_EMG.txt", header = FALSE)

## Function to generate array, given two lists and an array 
foodArray <- function(starta, endb, oriArray){
  l_a <- length(starta)
  newArray <- oriArray[c(starta[1]:endb[1]), ]
  for(x in 2:l_a) {
    addArray <- oriArray[c(starta[x]:endb[x]), ]
    newArray <- rbind(newArray, addArray)
  }
  return(newArray)
}

# Generate Eating (Fork) array using foodArray function
eatForkstart <- user09_fork_gt$V3
eatForkend <- user09_fork_gt$V4
eat_fork_array <- foodArray(eatForkstart, eatForkend, user09_EMG_fork)


# Generate  non-Eating (Fork) array by getting the extra rows in EMG array that is absent in the eating Array
non_eat_fork_array <- setdiff(user09_EMG_fork, eat_fork_array)

# Generate Eating (Spoon) array using foodArray function
eatSpoonstart <- user09_spoon_gt$V3
eatSpoonend <- user09_spoon_gt$V4
eat_spoon_array <- foodArray(eatSpoonstart, eatSpoonend, user09_EMG_spoon)


# Generate  non-Eating (Spoon) array by getting the extra rows in EMG array that is absent in the eating Array
non_eat_spoon_array <- setdiff(user09_EMG_spoon, eat_spoon_array)

#Add labells
eat_fork_array$V1 = "eating"
non_eat_fork_array$V1 = "non-eating"
eat_spoon_array$V1 = "eating"
non_eat_spoon_array$V1 = "non-eating"

# Generate two arrays of equal numbers of rows for eating and non_eating
non_eat_fork_array = non_eat_fork_array[c(1:2896), ]
non_eat_spoon_array = non_eat_spoon_array[c(1:3299), ]

eating09 <- rbind(eat_fork_array, eat_spoon_array)
non_eating09 <- rbind(non_eat_fork_array, non_eat_spoon_array)

# Save the arrays
write.table(eating09, file = "eating09.csv", sep = ",", row.names=FALSE, col.names=FALSE)
write.table(non_eating09, file = "non_eating09.csv", sep = ",", row.names=FALSE, col.names=FALSE)



##################
##              ##
##   PHASE 2    ##
##              ##
##################

##### FEATURE EXTRACTION #####

# Compute Mean of rows and adjoin to datasets
eating09$V10 <- rowMeans(eating09[c(2:9)])
non_eating09$V10 <- rowMeans(non_eating09[c(2:9)])

# Compute Median of rows and adjoin to datasets
# load installed package matrixStats
library(matrixStats)
eating09$V11 <- rowMedians(as.matrix(eating09[c(2:9)]))
non_eating09$V11 <- rowMedians(as.matrix(non_eating09[c(2:9)]))

# Compute std of rows and adjoin to datasets
eating09$V12 <- rowSds(as.matrix(eating09[c(2:9)]))
non_eating09$V12 <- rowSds(as.matrix(non_eating09[c(2:9)]))


# Compute Max of rows and adjoin to datasets
eating09$V13 <- rowMaxs(as.matrix(eating09[c(2:9)]))
non_eating09$V13 <- rowMaxs(as.matrix(non_eating09[c(2:9)]))


#Compute ICA
#Load installed package fastICA
library(fastICA)
# Combine arrays (sources) without the labells into vector variable S
# For ICA, I am using the reduced feature from mean
S <- cbind(eating09[10], non_eating09[10])
# Make mixing matrix
A <- matrix(rnorm(4), 2, 2)
# Mix two signals
X <- as.matrix(S) %*% A
# ICA for extracting independent sources from mixed signals
a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
#Adjoin ICA extracted features to datasets
eating09$V14 <- a$S[,1]
non_eating09$V14 <- a$S[,2]



##### PlOTS #####

# Plots for raw data - 8 EMG variabales
pdf("Phase2raw1.pdf") 
plot(1:6195, non_eating09[,2], type = "l", xlab = "Time", ylab = "EMG1", col="red")
lines(1:6195, eating09[,2], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw2.pdf") 
plot(1:6195, non_eating09[,3], type = "l", xlab = "Time", ylab = "EMG2", col="red")
lines(1:6195, eating09[,3], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw3.pdf") 
plot(1:6195, non_eating09[,4], type = "l", xlab = "Time", ylab = "EMG3", col="red")
lines(1:6195, eating09[,4], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw4.pdf") 
plot(1:6195, non_eating09[,5], type = "l", xlab = "Time", ylab = "EMG4", col="red")
lines(1:6195, eating09[,5], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw5.pdf") 
plot(1:6195, non_eating09[,6], type = "l", xlab = "Time", ylab = "EMG5", col="red")
lines(1:6195, eating09[,6], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw6.pdf") 
plot(1:6195, non_eating09[,7], type = "l", xlab = "Time", ylab = "EMG6", col="red")
lines(1:6195, eating09[,7], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw7.pdf") 
plot(1:6195, non_eating09[,8], type = "l", xlab = "Time", ylab = "EMG7", col="red")
lines(1:6195, eating09[,8], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

pdf("Phase2raw8.pdf") 
plot(1:6195, non_eating09[,9], type = "l", xlab = "Time", ylab = "EMG8", col="red")
lines(1:6195, eating09[,9], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 


# Plot for mean
pdf("Phase2mean.pdf") 
plot(1:6195, non_eating09[,10], type = "l", xlab = "Time", ylab = "Mean", col="red")
lines(1:6195, eating09[,10], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for median
pdf("Phase2median.pdf") 
plot(1:6195, non_eating09[,11], type = "l", xlab = "Time", ylab = "Median", col="red")
lines(1:6195, eating09[,11], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for std
pdf("Phase2std.pdf") 
plot(1:6195, non_eating09[,12], type = "l", xlab = "Time", ylab = "Standard Deviation", col="red")
lines(1:6195, eating09[,12], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 


# Plot for max
pdf("Phase2max.pdf") 
plot(1:6195, non_eating09[,13], type = "l", xlab = "Time", ylab = "Maxima", col="red")
lines(1:6195, eating09[,13], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for ica - extracted independent signals
pdf("Phase2ica.pdf") 
plot(1:6195, non_eating09[,14], type = "l", xlab = "Time", ylab = "ICA-Extracted", col="red")
lines(1:6195, eating09[,14], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 



##################
##              ##
##   PHASE 3    ##
##              ##
##################


# Subtask1: Generate feature arrays (in csv file) to meet rubrics requirement
pcaInput_eating <- eating09[c(1:6)]
pcaInput_eating[c(2:6)] <- eating09[c(10:14)]
pcaInput_non_eating <- non_eating09[c(1:6)]
pcaInput_non_eating[c(2:6)] <- non_eating09[c(10:14)]
write.table(pcaInput_eating, file = "pcaInput_eating.csv", sep = ",", row.names=FALSE, col.names=FALSE)
write.table(pcaInput_non_eating, file = "pcaInput_non_eating.csv", sep = ",", row.names=FALSE, col.names=FALSE)

# Subtask2.1: Perform PCA. 
#Prepare data from features, both eatinag and non eating
pcaInput <- rbind(pcaInput_eating[c(2:6)], pcaInput_non_eating[c(2:6)])
# Rename columns to reflect features
names(pcaInput) <- (c("Mean", "Median", "STD", "Max", "ICA"))
#Load base package stats
library(stats)
# do pca
eat_pca <- princomp(pcaInput, cor = TRUE, scores = TRUE)
# Subtask2.2: Obtain eigenvectors
eigenVs <- data.frame(eat_pca$loadings[1:5, 1:5])
write.table(eigenVs, file = "Phase3eigenvectors.csv", sep = ",", col.names=FALSE)
#Subtask2.3: Show the eigenvectors in a spider plot
#Load installed package fmsb
library(fmsb)
# Spider plot
pdf("Phase3_SpiderPlot.pdf") 
radarchart(data.frame(t(eigenVs)), seg=5, plty=1, title="Spider Plot of Eigenvectors", vlcex=0.5, maxmin=FALSE, pcol=c("red", "blue", "black", "purple", "brown")) 
legend("bottomright", legend=c("Component 1", "Component 2", "Component 3", "Component 4", "Component 5"), col=c("red", "blue", "black", "purple", "brown"), lty=1, lwd=2, cex=0.5)
dev.off()

#Subtask3.1:
#Summary information of the pca to know contribution of each component
summary(eat_pca)
#Subtask3.2:
#Generate new features
newFeatures <- eat_pca$scores
write.table(newFeatures, file = "Phase3newFeatures.csv", sep = ",", row.names=FALSE)

#Subtask4:
#Generate new features matrix
newFeatureMatrix <- eat_pca$scores[,1:3] %*% t(eat_pca$loadings[,1:3])
newFeatureMatrix <- scale(newFeatureMatrix, center = -eat_pca$center, scale = TRUE)
write.table(newFeatureMatrix, file = "Phase3newFeatureMatrix.csv", sep = ",", row.names=FALSE)

#Subtask5: Plots for features modified by pca
# Plot for mean
pdf("Phase3mean.pdf") 
plot(1:6195, newFeatureMatrix[1:6195,1], type = "l", xlab = "Time", ylab = "Mean", col="red")
lines(1:6195, newFeatureMatrix[6196:12390,1], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for median
pdf("Phase3median.pdf") 
plot(1:6195, newFeatureMatrix[1:6195,2], type = "l", xlab = "Time", ylab = "Median", col="red")
lines(1:6195, newFeatureMatrix[6196:12390,2], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for std
pdf("Phase3std.pdf") 
plot(1:6195, newFeatureMatrix[1:6195,3], type = "l", xlab = "Time", ylab = "Standard Deviation", col="red")
lines(1:6195, newFeatureMatrix[6196:12390,3], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for max
pdf("Phase3max.pdf") 
plot(1:6195, newFeatureMatrix[1:6195,4], type = "l", xlab = "Time", ylab = "Maxima", col="red")
lines(1:6195, newFeatureMatrix[6196:12390,4], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 

# Plot for ica
pdf("Phase3ica.pdf") 
plot(1:6195, newFeatureMatrix[1:6195,5], type = "l", xlab = "Time", ylab = "ICA", col="red")
lines(1:6195, newFeatureMatrix[6196:12390,5], col="green")
legend("topright", legend=c("Eating", "Non-eating"), col=c("green", "red"), lty=1)
dev.off() 





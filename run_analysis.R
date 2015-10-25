##Coursera Getting and Cleaning Data Course Project

##Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##1) Merges the training and the test sets to create one data set.
##2) Extracts only the measurements on the mean and standard deviation for each measurement. 
##3) Uses descriptive activity names to name the activities in the data set
##4) Appropriately labels the data set with descriptive variable names. 
##5) From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.

##1) Merges the training and the test sets to create one data set.
##Download data
library(RCurl)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="libcurl")
unzip(zipfile="./data/Dataset.zip",exdir="./data")
pathData <- file.path("./data" , "UCI HAR Dataset")

##Reads datafiles
features = read.table(file.path(pathData, "features.txt"), head=FALSE)
activityType = read.table(file.path(pathData, "activity_labels.txt"), head=FALSE)
subjectTrain = read.table(file.path(pathData, "train", "subject_train.txt"), header=FALSE)
xTrain = read.table(file.path(pathData, "train", "x_train.txt"), header=FALSE)
yTrain = read.table(file.path(pathData, "train", "y_train.txt"), header=FALSE)

subjectTest = read.table(file.path(pathData, "test", "subject_test.txt"), header=FALSE)
xTest = read.table(file.path(pathData, "test", "x_test.txt"), header=FALSE)
yTest = read.table(file.path(pathData, "test", "y_test.txt"), header=FALSE)

##Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2] 
colnames(yTest)       = "activityId"

##Merges subjectTrain, xTrain, yTrain
trainingData = cbind(subjectTrain, xTrain, yTrain)
testData = cbind(subjectTest, xTest, yTest)
finalData = rbind(trainingData,testData);

##2) Extracts only the measurements on the mean and standard deviation for each measurement.

##Extracts column names from merged dataset
colNames = colnames(finalData)

##Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
##logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("mean\\(\\)|std\\(\\)",colNames))

##Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE]

##3) Uses descriptive activity names to name the activities in the data set

##Merge finalData with acitivityType to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)

##Update colNames to include the new column names after merge
colNames  = colnames(finalData)

##4) Appropriately labels the data set with descriptive variable names. 

names(finalData)<-gsub("^t", "time", names(finalData))
names(finalData)<-gsub("^f", "frequency", names(finalData))
names(finalData)<-gsub("Acc", "Accelerometer", names(finalData))
names(finalData)<-gsub("Gyro", "Gyroscope", names(finalData))
names(finalData)<-gsub("Mag", "Magnitude", names(finalData))
names(finalData)<-gsub("BodyBody", "Body", names(finalData))

##Update colNames to include the new column names after labeling with descriptive variable names
colNames  = colnames(finalData)

##5) From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.
library(plyr);

tidyData <- aggregate(. ~subjectId + activityId, finalData, mean)
tidyData <- tidyData[order(tidyData$subjectId,tidyData$activityId),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE)
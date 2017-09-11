## Final Project for "Getting and Cleaning Data Course"
## You will be graded by your peers on a series of yes/no questions related to the project. 
##You will be required to submit: 1) a tidy data set as described below, 
## 2) a link to a Github repository with your script for performing the analysis, and 
## 3) a code book that describes the variables, the data, and any transformations or 
## work that you performed to clean up the data called CodeBook.md
##
## Here is the data for the project:

## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

##You should create one R script called run_analysis.R that does the following: 
##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##5. From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setwd("C:/Users/Sergio/courseraRprog/UCI HAR Dataset/")
dirpath<- getwd()

## Stpe 1: Download the dataset and unzip to a folder in the project directory. 
## Important: I am using here a directory associated with my coursera class !!!!
## 
##  download.file(Url,destfile="./projectData/project_Dataset.zip",mode = "wb")
## if(!file.exists("./projectData/UCI HAR Dataset")){
## unzip(zipfile="./projectData/project_Dataset.zip",exdir="./projectData")

files<-list.files(dirpath, recursive=TRUE)

## The files needed according to instructions are:
# test/subject_test.txt
# test/X_test.txt
# test/y_test.txt
# train/subject_train.txt
# train/X_train.txt
# train/y_train.txt

## Stpe 2: Read data from the files into clearly named variables

## 2.1.Activity files (remember that they are under two subdirectories: text and train)

ActivityTest  <- read.table(file.path(dirpath, "test" , "Y_test.txt" ),header = FALSE)
ActivityTrain <- read.table(file.path(dirpath, "train", "Y_train.txt"),header = FALSE)

## 2.2. Subject files

SubjectTrain <- read.table(file.path(dirpath, "train", "subject_train.txt"),header = FALSE)
SubjectTest  <- read.table(file.path(dirpath, "test" , "subject_test.txt"),header = FALSE)

## 2.3. Features files

FeaturesTest  <- read.table(file.path(dirpath, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(dirpath, "train", "X_train.txt"),header = FALSE)
## Test: Look at data sets properties using str()

## Step 3: Merge the training and the test sets to create one data set (point #1 of Project)

## 3.1.Build one data table each for Subjects, Activity and Features, 
## adding rows for Train and Test 
dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity<- rbind(ActivityTrain, ActivityTest)
dataFeatures<- rbind(FeaturesTrain, FeaturesTest)

## 3.2 Give names to datasets columns using "suject", Activity", 
## and the names in file "fetaures.txt" for dataFeatures
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(dirpath, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

## 3.3. Bind data sets by columns to get one data frame for all data
dataCombine <- cbind(dataSubject, dataActivity)
allData <- cbind(dataFeatures, dataCombine)


## Step 4: Extracts only the measurements on the mean and standard deviation 
## for each measurement. (Point #2 of the project)

## 4.1. Subset dataFeaturesNames that contain "mean()" and "std()" as part of it
## Extract using first grep to identify the indices, and then apply it
partdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

## 4.2. Create list of selected column names, plus "subject" and "activity" 
## to use select an allData frame
selectedNames<-c(as.character(partdataFeaturesNames), "subject", "activity" )
Data<-subset(allData,select=selectedNames)

## Step 5: Use descriptive activity names to name the activities in the data set (Point 3 of project)

## 5.1. Read descriptive activity names from "activity_labels.txt" file
activityLabels <- read.table(file.path(dirpath, "activity_labels.txt"),header = FALSE)

## 5.2. Assign labels to substitute numeric values of Variable "activity" using 
## descriptive activity names imported just above
Data$activity<-factor(Data$activity,labels=activityLabels[,2])

## Step 6: Appropriately labels the data set with descriptive variable names (Point 4 of project)

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

## check the resulT: names(Data)

## Step 7: From the data set in point 4 of the project, create a second, ordered
## independent tidy data set, with the average of each variable for each subject and acivity.
tidyData<-aggregate(. ~subject + activity, Data, mean)
tidyData<-tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "tidydata.txt",row.name=FALSE,quote = FALSE, sep = '\t')

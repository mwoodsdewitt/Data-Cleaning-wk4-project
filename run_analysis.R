library(dplyr)
library(readr)


##############################################################################
#Get data
##############################################################################
mainDir <- "C:/"
subDir <- "library"
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}
# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "C:/library/UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "C:/library/UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile, exdir= "C:/library")
}
files = list.files(path="C:/library/UCI HAR Dataset", recursive = TRUE)
#Reading training tables
x_train = read.table(file.path(dataPath, "train", "X_train.txt"),header = FALSE)
y_train = read.table(file.path(dataPath, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(dataPath, "train", "subject_train.txt"),header = FALSE)
#Reading testing tables
x_test = read.table(file.path(dataPath, "test", "X_test.txt"),header = FALSE)
y_test = read.table(file.path(dataPath, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(dataPath, "test", "subject_test.txt"),header = FALSE)
#Reading features data
features = read.table(file.path(dataPath, "features.txt"),header = FALSE)
#Reading activity labels data
activityLabels = read.table(file.path(dataPath, "activity_labels.txt"),header = FALSE)

#Create Column Values for the Train Data
colnames(x_train) = features[,2]
colnames(y_train) = "activityId"
colnames(subject_train) = "subjectId"
#Create column values for the test data
colnames(x_test) = features[,2]
colnames(y_test) = "activityId"
colnames(subject_test) = "subjectId"
#Create check for the activity labels value
colnames(activityLabels) <- c('activityId','activityType')

#Merging the training and test data
mrg_train = cbind(y_train, subject_train, x_train)
mrg_test = cbind(y_test, subject_test, x_test)
#Create the main data table merging both table tables
setAll = rbind(mrg_train, mrg_test)

# remove individual data tables to save memory
rm(y_train, subject_train, x_train, 
   y_test, subject_test, x_test, mrg_test, mrg_train)

# Need step is to read all the values that are available
colNames = colnames(setAll)
#Need to get a subset of all the mean and standards and the correspondongin activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
#A subtset has to be created to get the required dataset
set_MeanAndStd <- setAll[ , mean_and_std == TRUE]
set_ActivityNames = merge(set_MeanAndStd, activityLabels, by='activityId', all.x=TRUE)

# Cleaning and clairifying variable names
# get column names
set_ActivityNames_Cols <- colnames(set_ActivityNames)
# removing common special characters
set_ActivityNames_Cols <- gsub("[\\(\\)-]", "", set_ActivityNames_Cols)

# expanding abbreviations and clean up names
set_ActivityNames_Cols <- gsub("^f", "Frequency_Domain", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("^t", "Time_Domain", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("Acc", "Accelerometer", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("Gyro", "Gyroscope", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("Mag", "Magnitude", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("Freq", "Frequency", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("mean", "Mean", set_ActivityNames_Cols)
set_ActivityNames_Cols <- gsub("std", "Standard_Deviation", set_ActivityNames_Cols)

# Replacing old column names with updted labels
colnames(set_ActivityNames) <- set_ActivityNames_Cols

tidy_set <- aggregate(. ~subjectId + activityId, set_ActivityNames, mean)
tidy_set <- tidy_set[order(tidy_set$subjectId, tidy_set$activityId),]
write.table(tidy_set, "C:/library/tidy_data.txt", row.name=FALSE,col.names = TRUE)

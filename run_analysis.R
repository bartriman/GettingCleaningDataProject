## 1. Preparing data (download and unzip)
## 2. Create union of training and the test sets
## 3. Create data set containg descriptive activity names and measurements on the mean and standard deviation
## 4. Create separate, clean data set with the average of each variable for each activity and each subject

#load library needed for running this script
library(dplyr)


## 1. Preparing data (download and unzip)

#download and unzip data
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
ZippedFile <- "Dataset.zip"
if (!file.exists(ZippedFile)) {
  download.file(Url, ZippedFile, mode = "wb")
}

DataPath <- "UCI HAR Dataset"
if (!file.exists(DataPath)) {
  unzip(ZippedFile)
}

# read in training data
trSub <- read.table(file.path(DataPath, "train", "subject_train.txt"))
trVal <- read.table(file.path(DataPath, "train", "X_train.txt"))
trAct <- read.table(file.path(DataPath, "train", "y_train.txt"))

# read in test data
tsSub <- read.table(file.path(DataPath, "test", "subject_test.txt"))
tsVal <- read.table(file.path(DataPath, "test", "X_test.txt"))
tsAct <- read.table(file.path(DataPath, "test", "y_test.txt"))

features <- read.table(file.path(DataPath, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(DataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


## 2. Create union of training and the test sets

# Create union of training and the test sets
hAct <- rbind(
  cbind(trSub, trVal, trAct),
  cbind(tsSub, tsVal, tsAct)
)
colnames(hAct) <- c("subject", features[, 2], "activity")


## 3. Create data set containg descriptive activity names and measurements on the mean and standard deviation

# Measurements on the mean and standard deviation for each measurement
MeanSDcol <- grepl("subject|activity|mean|std", colnames(hAct))
hAct <- hAct[, MeanSDcol]

# Assiging descriptive activity names to name the activities in the data set
hAct$activity <- factor(hAct$activity, levels = activities[, 1], labels = activities[, 2])
hActCols <- colnames(hAct)
hActCols <- gsub("[\\(\\)-]", "", hActCols)
hActCols <- gsub("^f", "frequencyDomain", hActCols)
hActCols <- gsub("^t", "timeDomain", hActCols)
hActCols <- gsub("Acc", "Accelerometer", hActCols)
hActCols <- gsub("Gyro", "Gyroscope", hActCols)
hActCols <- gsub("Mag", "Magnitude", hActCols)
hActCols <- gsub("Freq", "Frequency", hActCols)
hActCols <- gsub("mean", "Mean", hActCols)
hActCols <- gsub("std", "StandardDeviation", hActCols)
hActCols <- gsub("BodyBody", "Body", hActCols)
colnames(hAct) <- hActCols


## 4. Create separate, clean data set with the average of each variable for each activity and each subject

# group by subject and activity and calculate mean
hActMeans <- hAct %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(hActMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)









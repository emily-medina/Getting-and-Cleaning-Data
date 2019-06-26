#load package
library(dplyr)
#download data
filename <- "Coursera_DS3_Final.zip"
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}
#assign dataframes
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# now the data is clean and ready to start working with it
# 1. Merges the training and the test sets to create one data set.
x <- rbind(xtrain, xtest)
y <- rbind(ytrain, ytest)
subject <- rbind(subjecttrain, subjecttest)
mergeddata <- cbind(subject, y, x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
cleandata <- mergeddata %>% select(subject, code, contains("mean"), contains("std"))

# 3.Uses descriptive activity names to name the activities in the data set

cleandata$code <- activitylabels[cleandata$code, 2]

#4. Appropriately labels the data set with descriptive variable names.

names(cleandata)[2] = "activity"
names(cleandata)<-gsub("Acc", "Accelerometer", names(cleandata))
names(cleandata)<-gsub("Gyro", "Gyroscope", names(cleandata))
names(cleandata)<-gsub("BodyBody", "Body", names(cleandata))
names(cleandata)<-gsub("Mag", "Magnitude", names(cleandata))
names(cleandata)<-gsub("^t", "Time", names(cleandata))
names(cleandata)<-gsub("^f", "Frequency", names(cleandata))
names(cleandata)<-gsub("tBody", "TimeBody", names(cleandata))
names(cleandata)<-gsub("-mean()", "Mean", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("-std()", "STD", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("-freq()", "Frequency", names(cleandata), ignore.case = TRUE)
names(cleandata)<-gsub("angle", "Angle", names(cleandata))
names(cleandata)<-gsub("gravity", "Gravity", names(cleandata))

#5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

seconddata <- cleandata %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(seconddata, "FinalAssignmentData.txt", row.name=FALSE)






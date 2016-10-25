##############################################################################################################################
## START
##############################################################################################################################

## REVIEW CRITERIA

# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book (called codebook.MD) that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, 
# along with units, and any other relevant information.
# The README.md that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.

# DETAIL

# One of the most exciting areas in all of data science right now is wearable computing - http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/
# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 

# The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained:

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Here are the data for the project:

# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# You should create one R script called run_analysis.R that does the following.

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##############################################################################################################################

## Merge training and test data + add in descriptive activity labels (see lines activityMerged variable below)

# Download and save the raw data

library(dplyr)

setwd("C:/Users/will.granger/Dropbox/Training/Johns Hopkins Data Science/Get Clean Data wk 4/Course Project")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "project data.zip"
download.file(url, file)

unzip(file, list = TRUE) ## see contents of zip file

unzip(file, exdir = "UCI HAR Dataset - unzipped project data")

## Training data

# Read in the data

dataDir <- ("./UCI HAR Dataset - unzipped project data")
features <- read.table(paste(dataDir,"features.txt",sep="/"), header = FALSE)
activityType <- read.table(paste(dataDir,"activity_labels.txt",sep="/"), header = FALSE)
subjectTrain <- read.table(paste(dataDir,"train/subject_train.txt",sep="/"), header = FALSE)
trainingSet <- read.table(paste(dataDir,"train/x_train.txt",sep="/"), header = FALSE)
trainingLabels <- read.table(paste(dataDir,"train/y_train.txt",sep="/"), header = FALSE)

# Rename the columns

names(trainingSet) <- features[,2]
names(subjectTrain) <- "Subject"
names(trainingLabels) <- "ActivityID"
names(activityType) <- c("ActivityID","Activity")

# Merge into one dataset

trainingData <- cbind(trainingSet, trainingLabels, subjectTrain)
valid_column_names <- make.names(names=names(trainingData), unique=TRUE, allow_ = TRUE)
names(trainingData) <- valid_column_names
trainingData <- trainingData %>% inner_join(activityType) %>% select(-ActivityID) ## this adds the descriptive activity labels to each row, rather than using a factor number

# Clean up

rm(list = c("trainingSet","trainingLabels","subjectTrain", "valid_column_names"))

## Test data

# Read in the data 

subjectTest <- read.table(paste(dataDir,"test/subject_test.txt",sep="/"), header = FALSE)
testSet <- read.table(paste(dataDir,"test/X_test.txt",sep="/"), header = FALSE)
testLabels <- read.table(paste(dataDir,"test/y_test.txt",sep="/"), header = FALSE)

# Rename the columns

names(testSet) <- features[,2]
names(subjectTest) <- "Subject"
names(testLabels) <- "ActivityID"

# Merge into one dataset

testData <- cbind(testSet, testLabels, subjectTest)
valid_column_names <- make.names(names=names(testData), unique=TRUE, allow_ = TRUE)
names(testData) <- valid_column_names
testData <- testData %>% inner_join(activityType) %>% select(-ActivityID) ## this adds the descriptive activity labels to each row, rather than using a factor number

## Make combined test + training dataset

allData <- rbind(testData, trainingData)

# Clean up

rm(list = c("testSet","testLabels","subjectTest","features","activityMerged","activityType","testData","trainingData"))

##############################################################################################################################

## Select only the mean and standard deviation variables

varsToKeep <- grep("mean\\.|std\\.", names(allData)) ## searching only for 'mean' brings up the meanFreq variables as well, so escape characters needed to search for 'mean()'
varsToKeep <- c(varsToKeep, 562, 563) ## we also need to keep the 'names' and 'activity' columns

meanAndStd <- allData[,varsToKeep]

# Clean up

rm(allData)

##############################################################################################################################

## Label the variable names

colNames <- names(meanAndStd)

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(meanAndStd) <- colNames ## apply the new column names

##############################################################################################################################

## New tidy dataset averaging each variable by activity and subject

meanAndStdSummary <- meanAndStd
meanAndStdSummary$Subject <- factor(meanAndStdSummary$Subject)

meanAndStdSummary %>% group_by(Subject, Activity) %>% summarise() %>% select(Activity, Subject) ## confirming there are 40 unique combinations of subject + activity
allData %>% group_by(Subject, Activity) %>% summarise() %>% select(Activity, Subject) ## confirming there are 40 unique combinations of subject + activity

meanAndStdSummary <- meanAndStdSummary %>%
  group_by(Activity, Subject) %>%
  summarise_each(funs(mean))

write.table(meanAndStdSummary, "final_table.txt", row.names = FALSE)

##############################################################################################################################
## END
##############################################################################################################################

library(plyr)
library(dplyr)
library(data.table)


setwd("C:/Users/john.mcphillips/Google Drive/Coursera/Getting and Cleaning Data/CourseProject/FUCI HAR Dataset/UCI HAR Dataset")

# Read all required files in to appropriately named data sets
features <- read.table("features.txt", sep = '', as.is = TRUE, col.names = c("FeatureId", "Feature"))
activites <- read.table("activity_labels.txt", sep = '', as.is = TRUE, col.names = c("ActivityId", "ActivityLabel"))

TestSubjects <- read.table("test/subject_test.txt", sep = '', col.names = "SubjectId")
TestSet <- read.table("test/X_test.txt", sep = '')
TestLabels <- read.table("test/y_test.txt", sep = '', col.names = "ActivityId")

TrainingSubjects <- read.table("train/subject_train.txt", sep = '', col.names = "SubjectId")
TrainingSet <- read.table("train/X_train.txt", sep = '')
TrainingLabels <- read.table("train/y_train.txt", sep = '', col.names = "ActivityId")

###################################################################################################
## Data preparation
###################################################################################################

# Assign the column names to the training and test sets
# These are listed in the features data frame. Subsetting the second 
#  column of this data frame returns a character vector with 561 members, 
#  the exact number of variables in the test and training data sets
colnames(TestSet) <- features[,2]
colnames(TrainingSet) <- features[,2]

# To get the full name , we need to join with the activities data set with
#  the labels data set
# I am using join from the plyr package because it preserves the order of the data set 
#  (unlike merge from dplyr), which is critical for this data set
TestLabels <- join(TestLabels, activites)
TrainingLabels <- join(TrainingLabels, activites)

#Combine the Subject, Labels and DataSet in to one data frame for Test and Training
#NOTE this will require the cbind function

CombinedTestSet <- cbind(TestLabels, TestSubjects, TestSet)
CombinedTraningSet <- cbind(TrainingLabels, TrainingSubjects, TrainingSet)

###################################################################################################
## 1. Merges the training and the test sets to create one data set.
###################################################################################################

FullDataSet <- rbind(CombinedTestSet, CombinedTraningSet)


###################################################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
###################################################################################################

# Search for the fields which contain mean or standard deviation and discard the others
# Using grepl to get a logical vector which contains TRUE for each column that contains
#  the search value
MeasurementsRequired <- grepl("ActivityLabel|SubjectId|mean|std", colnames(FullDataSet))

# Now we can use this vector to get a subset of the fields required
FullDataSet <- FullDataSet[,MeasurementsRequired]

###################################################################################################
## 3. Uses descriptive activity names to name the activities in the data set
###################################################################################################

# Not sure of the exact requirement for this step, but I have already merged the 
#  data set with activities data set to apply the names, so perhaps this requirement
#  is already satisfied by that


###################################################################################################
## 4. Appropriately labels the data set with descriptive variable names.
###################################################################################################

# Give the field names a more meaningful description.

# As described in features_info.txt:
#   t denotes time domani and f denotes frequency domain
#   Gyro is short for Gyroscope
#   Acc is short for Accelerometer
# I will replace these abbreviations in the field names with the full names
colnames(FullDataSet) <- gsub("^t", "time", colnames(FullDataSet))
colnames(FullDataSet) <- gsub("^f", "frequency", colnames(FullDataSet))
colnames(FullDataSet) <- gsub("Acc", "Accelerometer", colnames(FullDataSet))
colnames(FullDataSet) <- gsub("Gyro", "Gyroscope", colnames(FullDataSet))

# Some field names seem to have "BodyBody" which looks wrong. I think it should
#  be just "Body", so I will fix that as well
colnames(FullDataSet) <- gsub("BodyBody", "Body", colnames(FullDataSet))

# The brackets are not really necessary so I remove them, making sure to escape
#  the bracket characters with \\ because they are special characters in search patterns
# Also making mean and std upper case for first letter and 
#  removing the "-" to maintain camel case naming convention

colnames(FullDataSet) <- gsub("-mean\\(\\)", "Mean", colnames(FullDataSet))
colnames(FullDataSet) <- gsub("-std\\(\\)", "Std", colnames(FullDataSet))
colnames(FullDataSet) <- gsub("-meanFreq\\(\\)", "MeanFreq", colnames(FullDataSet))

###################################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#  average of each variable for each activity and each subject.
###################################################################################################


# Using data.table to group by Activity and Subject, with .SD to specify the subset of each data field
TidyDataSet <- setDT(FullDataSet)[, lapply(.SD, mean), by = list(SubjectId, ActivityLabel)]

write.table(TidyDataSet, "TidyDataSet.txt", row.names = FALSE, quote = FALSE)


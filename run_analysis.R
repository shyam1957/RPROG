#Shyam's solution to project assignment September 2015
library(dplyr)
library(tidyr)
library(data.table)

# If the dataset is not present in the current working directory then it has to be download. Download it.
sspUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("UCI HAR Dataset")) {
  if(!file.exists("sspdata")) {
    dir.create("sspdata")
  } 
  download.file(sspUrl, destfile = "sspdata/har.zip")
  unzip("sspdata/har.zip")
}
# Step 1 - Merges the training and the test sets to create one data set.
# merging training data
training_x <- read.table ("UCI HAR Dataset//train/X_train.txt", nrows = 7352, comment.char = "")
training_subject <- read.table("UCI HAR Dataset//train/subject_train.txt", col.names = c("subject"))
training_y <- read.table("UCI HAR Dataset//train/y_train.txt", col.names = c("activity"))
training_data <- cbind(training_x, training_subject, training_y)

# merging test data
test_x <- read.table("UCI HAR Dataset//test/X_test.txt", nrows = 2947, comment.char = "")
test_subject <- read.table("UCI HAR Dataset//test/subject_test.txt", col.names = c("subject"))
test_y <- read.table("UCI HAR Dataset//test/y_test.txt", col.names = c("activity"))
test_data <- cbind(test_x,test_subject,test_y)

#merging both training and test data
mergeddata <- rbind(training_data, test_data)

# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
#reads features whose measurements on mean and standard deviation are to be extracted.
measures_list <- read.table("UCI HAR Dataset//features.txt", col.names = c("id", "names"))
features <- c(as.vector(measures_list[ , "names"]), "subject", "activity")

# Extracts only those has mean or standard deviation in the name
extracted_feature_ids <- grepl("mean|std|subject|activity", features) & !grepl("meanFreq", features)
extracted_data = mergeddata[, extracted_feature_ids]

# Step 3 - Uses descriptive activity names to name the activities in the data set
activities <- read.table("UCI HAR Dataset//activity_labels.txt", col.names = c("id", "name"))
for (i in 1: nrow(activities)) { 
  extracted_data$activity[extracted_data$activity == activities[i, "id"]] <- 
    as.character(activities[i, "name"])
}

# Step 4 - Appropriately labels the data set with descriptive variable names.
# makes feature names easy to read
extracted_feature_names  <- features[extracted_feature_ids]
extracted_feature_names  <- gsub("\\(\\)", "", extracted_feature_names)
extracted_feature_names  <- gsub("Acc", "-acceleration", extracted_feature_names )
extracted_feature_names  <- gsub("Mag", "-Magnitude", extracted_feature_names )
extracted_feature_names  <- gsub("^t(.*)$", "\\1-time", extracted_feature_names )
extracted_feature_names  <- gsub("^f(.*)$", "\\1-frequency", extracted_feature_names )
extracted_feature_names  <- gsub("(Jerk|Gyro)", "-\\1", extracted_feature_names )
extracted_feature_names  <- gsub("BodyBody", "Body", extracted_feature_names )
extracted_feature_names  <- tolower(extracted_feature_names )
# assigning names to features
names(extracted_data) <- extracted_feature_names 

# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- tbl_df (extracted_data) %>%
  group_by ('subject', 'activity') %>%
  summarise_each(fun(mean)) %>%
  gather(measurement, mean, -activity, -subject)
# save the data into the file
write.table (tidy_data, file = "tidy_data.txt", row.name = FALSE)


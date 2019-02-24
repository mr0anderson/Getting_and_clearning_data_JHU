# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set 
    #     with the average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)
library(readr)
library(data.table)

run_analysis <- function() {
  
  # FEATURES file, only take mean() and std() variables from features.txt and place into a new vector
  features <- read.table("./UCI HAR Dataset/features.txt");
  #place of mean variable in list
  mean_vect <- grep(x = features[, "V2"], pattern = "mean")
    #actual text print of the variable for column naming purpose
    mean_vars <- grep(x = features[, "V2"], pattern = "mean", value = TRUE)
      std_vect <- grep(x = features[, "V2"], pattern = "std")
        std_vars <- grep(x = features[, "V2"], pattern = "std", value = TRUE)
  mean_std_vect <- as.numeric(c(mean_vect, std_vect));
    #combine mean and sd into a sorted list as in the features.txt file
    mean_std_vars <- c(mean_vars, std_vars);
      vect_vars <- as.data.frame(cbind(mean_std_vect, mean_std_vars));
        vect_vars <- transform(vect_vars, mean_std_vect = as.numeric(levels(mean_std_vect)[mean_std_vect]))
          vect_vars <- arrange(vect_vars, mean_std_vect)
  
  # TEST files, take only mean and std features and combine with subject and activity        
  xtest <- read.delim("./UCI HAR Dataset/test/X_test.txt", sep="", header = FALSE, dec = ".", check.names = TRUE, na.strings = "", stringsAsFactors = F) 
    #select only the mean and std variables from the x_test data
    xtest_select <- select(xtest, vect_vars[,1])
      #rename the columns in x_test appropriately
      xtest_select <- setnames(xtest_select, old = names(xtest_select), new = as.character(vect_vars[,2]))

  ytest <- read.table("./UCI HAR Dataset/test/y_test.txt")
    colnames(ytest)[colnames(ytest)=="V1"] <- "activity"
      #replace the numeric values for activities in y_test with the corresponding activity from activity_labels.txt
      ytest$activity <- as.factor(ytest$activity)
        name_vec <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
         levels(ytest$activity) <- name_vec
  test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    colnames(test_subject)[colnames(test_subject)=="V1"] <- "subject"
  #create a 'test' dataframe that combines the subject, activity and the mean and std variables
  test <- cbind(test_subject, ytest, xtest_select)
  
  # TRAIN files, similar procedure as above for 'test' data frame
  xtrain <- read.delim("./UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE, dec = ".", check.names = TRUE, na.strings = "", stringsAsFactors = F)
  xtrain_select <- select(xtrain, vect_vars[,1])
  xtrain_select <- setnames(xtrain_select, old = names(xtrain_select), new = as.character(vect_vars[,2]))
  
  ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
    colnames(ytrain)[colnames(ytrain)=="V1"] <- "activity"
     ytrain$activity <- as.factor(ytrain$activity)
      name_vec <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
       levels(ytrain$activity) <- name_vec
  train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    colnames(train_subject)[colnames(train_subject)=="V1"] <- "subject"
  train <- cbind(train_subject, ytrain, xtrain_select);
  
  #combine train and test data into new data set, with appropriate names for mean and std columns
  learn <- rbind(test, train)
  learn <- arrange(learn, subject, activity)
  
  # from 'learn', independent tidy data set with the average of each variable for each activity and each subject
  # where ".~" in aggregate indicates to take the mean value of every variable other than subject and activity
  avg_activ <- aggregate(.~subject+activity, learn, mean)
  
  # alternatively using dplyr
  avg_activ_plyr <- group_by(learn, subject, activity)
  avg_activ_plyr <- summarise_all(avg_activ_plyr, funs(mean))
}
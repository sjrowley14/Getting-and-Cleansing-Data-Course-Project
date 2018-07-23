## Getting and Cleansing Data Course Project

  ## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

    # One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
    #   
    #   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
    # 
    # Here are the data for the project:
    #   
    #   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
    # 
    # You should create one R script called run_analysis.R that does the following.
    # 
    # Merges the training and the test sets to create one data set.
    # Extracts only the measurements on the mean and standard deviation for each measurement.
    # Uses descriptive activity names to name the activities in the data set
    # Appropriately labels the data set with descriptive variable names.
    # From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



  ## Setup
    
    packages <- c("data.table", "reshape2")
    sapply(packages, require, character.only = TRUE, quietly = TRUE)
    
    # Melt/dcast masked
    
    setwd("~/GitHub/Getting and Cleansing Data/Course Project")
  
    filepath <- getwd()
  
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    f <- "Dataset.zip"
    if (!file.exists(filepath)) {
      dir.create(filepath)
    }
    download.file(url, file.path(filepath, f))    
    
  ## Unzip (copied code from web)
    
    dataPath <- "UCI HAR Dataset"
    if (!file.exists(dataPath)) {
      unzip(f)
    }
    
    pathIn <- file.path(filepath, "UCI HAR Dataset")
    list.files(pathIn, recursive = TRUE)
    
  ## Read the subject files
    
    
    # read test data
    testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
    testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
    testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))
    
    # read training data
    trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
    trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))
    trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
    
    #activity labels
    activities <- read.table(file.path(dataPath, "activity_labels.txt"))
    colnames(activities) <- c("activityId", "activityLabel")
    
    #features
    features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
    
  ## Step 1. Merge
    
    # combine tables to make single data table
    humanActivity <- rbind(
      cbind(trainingSubjects, trainingValues, trainingActivity),
      cbind(testSubjects, testValues, testActivity)
    )
    
    str(humanActivity)
    
    # assign column names
    colnames(humanActivity) <- c("subject", features[, 2], "activity")
    
  ## Step 2. Extract only the measurements for mean and s.d. for each measurement
    
    # columns of data set to keep using column name
    columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
    
    # keep data in these columns only
    humanActivity <- humanActivity[, columnsToKeep]

  ## Step 3. Use descriptive activity names to name the activities in the data set
    
    # replace activity values with named factor levels
    humanActivity$activity <- factor(humanActivity$activity, 
                                     levels = activities[, 1], labels = activities[, 2])
    
  ## Step 4. Appropriately label the data set with descriptive variable names
    
    # get column names
    humanActivityCols <- colnames(humanActivity)
    
    # remove special characters
    humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
    
    # correct typo
    humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
    
    # expand abbreviations and clean up names
    humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
    humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
    humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
    humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
    humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
    humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
    humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
    humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
    
    # use new labels as column names
    colnames(humanActivity) <- humanActivityCols

  ## Step 5. Create a second, independent tidy set with the average of each
  ##         variable for each activity and each subject
    
    install.packages("magrittr")
    library(magrittr)
    library(dplyr)
    
    humanActivityMeans <- humanActivity %>% 
      group_by(subject, activity) %>%
      summarise_each(funs(mean))
    
    # output to file "tidy_data.txt"
    write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
                quote = FALSE)
    
    
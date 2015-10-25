run_analysis <- function(){
  # Author:        Patrick Ericx
  # Course:        Coursera - Data Science Track - 03.Data Cleaning
  # Exercise:      Project
  # Creation date: 24 oct 2015
  
  # This questions are solved by this script, in this order
  # Question 3: Uses descriptive activity names to name the activities in the data set
  # Question 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  # Question 4: Appropriately labels the data set with descriptive variable names. 
  # Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  activity_labels <- read.table(file="C:/Coursera/03_DataCleaning/project/code/datacleaningdownload/UCI HAR Dataset/activity_labels.txt")
  
  # Question 3: Uses descriptive activity names to name the activities in the data set
  colnames(activity_labels) <- c("label_ID", "Description")
  y <- read.table( file = "mergedDataSets/y.txt")
  colnames(y) <- c("label_ID", "datasource")
  y <- merge(y, activity_labels, "label_ID")   # inner join, there are no data without a match, not in y, not in activity_labels
  # this file was created manually in excel, after applying some filters, and then creating a csv as output for this r-script.
  keepColumns <- read.csv("columnsToKeep.csv", header = TRUE, sep=";")
  # we give the columns the same V<number> name to join them easier.
  
  # Question 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
  keepColumns$columnid <- paste("V", keepColumns$function_id, sep="")
  Xfilter <- X[, keepColumns$columnid]
  write.table(Xfilter, "mergedDataSets/X_mean_std.txt")
  write.table(y, "mergedDataSets/y_activity.txt")
 
  # Question 4: Appropriately labels the data set with descriptive variable names. 
  keepColumns[,2] <- sapply(keepColumns[,2], as.character)  # convert the measurement Factor to character. You can only assign character to colnames.
  colnames(Xfilter) <- keepColumns$measurement
  write.table(Xfilter, "mergedDataSets/X_reducedColumns.txt")
  subjectDescriptionData <- merge.all(subject,y,Xfilter)               # not a standard function !
  subjectDescriptionData$datasource.y <- NULL   # remove column
  subjectDescriptionData$label_ID <- NULL # we only want to keep the Description column
  library(plyr)
  renamedDF <- rename(subjectDescriptionData, c("V1" = "Subject", "datasource.x" = "Dataset", "Description" = "Activity"))
  renamedDF$Dataset <- NULL                      # the column dataset is obsolete
  write.table(renamedDF, "mergedDataSets/allDataInOneTable.txt")
  library(dplyr)
  
  # Question 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  tidydataset <- renamedDF %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
  write.table(tidydataset, file = "mergedDataSets/tidydatasetWithoutRowNames.txt", row.names = FALSE)
  
}
 
 merge.all <- function(x, ..., by = "row.names") {
 # http://stackoverflow.com/questions/22617593/merge-multiple-data-frames-by-row-names
 # This is required if you want to merge more then two datasets together, based on the row name.
 # if you first merge dataset 1 + dataset 2, then the row names get messed up, and you can not use that output to merge it with dataset 3.
 # this approach avoids that problem
 
  L <- list(...)
  for (i in seq_along(L)) {
    x <- merge(x, L[[i]], by = by)
    rownames(x) <- x$Row.names
    x$Row.names <- NULL
  }
  return(x)
}


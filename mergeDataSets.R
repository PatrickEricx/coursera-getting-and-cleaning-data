mergeDataSets <- function(){
  # Author:        Patrick Ericx
  # Course:        Coursera - Data Science Track - 03.Data Cleaning
  # Exercise:      Project
  # Creation date: 18 oct 2015
  
  # Source: a downloaded and extracted zip file with "test" and "training" datasets.
  #         1 textfile ProjectDataCleaning_filenamesInZip.txt with the complete path to all the extracted files that must be handled.
  # Output: 1 merged dataset containing the data from both test and training datasets. 
  # Folder: TBD
  
  extractedDataPaths_FileName <- "ProjectDataCleaning_filenamesInZip.txt"
  # from all the extracted files, we only take those that are inside one of the folders included in mergeNames, to avoid merging the "readme" etc.
  mergeNames <- c("test", "train")
  filesToHandle <- read.delim(extractedDataPaths_FileName, header = FALSE)
  colnames(filesToHandle) <- c("DataFilePath")
  # we convert the class(filesToHandle) from Factor to character. This is required for dir.exists.
  filesToHandle[] <- lapply(filesToHandle, as.character)
  # we add a new column to our dataframe.
  filesToHandle$isFolder <- dir.exists(filesToHandle$DataFilePath)
  # we add a new column to indicate which dataset it belongs to, and initialize it with a default value.
  filesToHandle$dataset <- 'NONE'  
  filesToHandle$dataset[grep(mergeNames[1], filesToHandle$DataFilePath)] <- mergeNames[1] # test
  filesToHandle$dataset[grep(mergeNames[2], filesToHandle$DataFilePath)] <- mergeNames[2] # train
  filesToHandle$mergedFileName <- "NONE"  # we create a new column, that we will use to store the merge into filename.
  # for every file create a new filename: remove the folder and the _test from the filename.
  filesToHandle$mergedFileName <- apply(filesToHandle[,c("DataFilePath", "dataset")], 1, function(y) getMergedIntoFilename(y["DataFilePath"], y["dataset"]))
  
  # after reading on the forums, it appeared that we do not need all the files in the Inertial folder, so we create a manual loop to go through the 3 remaining files.
  # https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment/
dir.create("mergedDatasets")    
  dataFilesToRetain <- c("subject.txt", "X.txt", "y.txt")
   
   train1 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/train/subject_train.txt", sep="")
   train2 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/train/X_train.txt", sep="")
   train3 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/train/y_train.txt", sep="")
   test1 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/test/subject_test.txt", sep="")
   test2 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/test/X_test.txt", sep="")
   test3 <- read.table(file = "datacleaningdownload/UCI HAR Dataset/test/y_test.txt", sep="")
  
   # We add an extra column to keep track from where the came after it is merged, so we can trace back to the source in case of problems.
   test1$datasource <- "test"
   test2$datasource <- "test"
   test3$datasource <- "test" 
   train1$datasource <- "train"
   train2$datasource <- "train"
   train3$datasource <- "train"
   
    subject <- rbind(train1, test1)
	X <- rbind(train2, test2)
	y <- rbind(train3, test3)
	  
	 write.table(X, file = "mergedDataSets/X.txt")
	 write.table(subject, file = "mergedDataSets/subject.txt")
     write.table(y, file = "mergedDataSets/y.txt")

}  

# required before calling the first time str_locate
   install.packages("stringr")
   library(stringr)

getMergedIntoFilename <- function(filenameIn, datasetName){
# this function returns the reduced file name that will be used for writing the merged output
   filename <- filenameIn
   folderSearch <- paste("/", datasetName, "/", sep = "")
   removeText <- paste("_" , datasetName, sep = "")
   # this returns an array with begin and end position. we are only interested in the end position.
   # we only execute this function if the dataset is known (test or train), so 

   pos <- str_locate(filenameIn, folderSearch)  
   filename <- substring(filename, pos[2])
   filename <- sub(removeText, filename, replace = "")
   filename  
}

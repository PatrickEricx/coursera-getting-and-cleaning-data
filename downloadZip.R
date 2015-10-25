downloadZip <- function() {
  # Author:        Patrick Ericx
  # Course:        Coursera - Data Science Track - 03.Data Cleaning
  # Exercise:      Project
  # Creation date: 15 oct 2015
  
  # The zip file is downloaded from the requested website and extracted in your home directory in a folder that we control.
  # We also create a txt file that contains all the file- and foldernames from within the zip.
  # By doing so, we can -in an other function- fetch and check from this list item per item if we have handled it or not.
  
  url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  extractdir <- "datacleaningdownload"
  outputFileName <- "ProjectDataCleaning_filenamesInZip.txt"
  
  zipfile <- basename(url)
  # print(zipfile)
  if (!file.exists(zipfile)){
    download.file(url, zipfile)
  } 
  
  if (!dir.exists(extractdir)){
    dir.create(extractdir)
  }
  
  unzip(zipfile, overwrite = TRUE, exdir = extractdir )
  filenamesInZip <- list.files(extractdir, full.names=TRUE, recursive=TRUE, include.dirs=TRUE)

  # Create one output file that contains the complete filenames from the zip. 
  fileConn<-file(outputFileName)
  # the file gets recreated every time, no append.
  writeLines(as.character(filenamesInZip),fileConn)
  close(fileConn) 
}


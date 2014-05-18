complete <- function(directory, id = 1:332) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'id' is an integer vector indicating the monitor ID numbers
     ## to be used
     
     ## Return a data frame of the form:
     ## id nobs
     ## 1  117
     ## 2  1041
     ## ...
     ## where 'id' is the monitor ID number and 'nobs' is the
     ## number of complete cases

     ## set working directory from input directory
     charSaveWD<-getwd()
     setwd(directory)
     
     ## set filename extension since we know this a CSV file
     charFileExtension <- ".csv"
     charFileExtension
     
     ## convert numeric vector to string vector of 3 characters each
     charFileID <- sprintf("%03d", id)
     charFileID
     
     ## combine FileID and FileExtension into Filename for read
     charFileName <- paste(charFileID, charFileExtension, sep="")
     charFileName
     
     ## initialize loop counter for use in accessing data files
     intLoopCounter <- 0
     
     ## initialize output data frame
     matrixAllData <- data.frame(id=NA,nobs=NA)[numeric(0),]
     
     ## Loop through the input number of ids.
     for (intLoop in id) {
          
          ## read the actual file
          intLoopCounter <- intLoopCounter + 1
          matrixFileData <- read.csv(charFileName[intLoopCounter]) 
          matrixFileData
          matrixAllData[intLoopCounter,1]<- intLoop
          matrixNoNullCol2<-subset(matrixFileData, !is.na(matrixFileData[2]))
          matrixComplete<-subset(matrixNoNullCol2, !is.na(matrixNoNullCol2[3]))
          matrixAllData[intLoopCounter,2]<- nrow(matrixComplete)
     }
     
     ## set working directory back to original directory
     setwd(charSaveWD)
     
     ## return our data
     matrixAllData
     
}
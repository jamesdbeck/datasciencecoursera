pollutantmean <- function(directory, pollutant, id =1:332){
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'pollutant' is a character vector of length 1 indicating
     ## the name of the pollutant for which we will calculate the
     ## mean; either "sulfate" or "nitrate".
     
     ## Return the mean of the pollutant across all monitors list
     ## in the 'id' vector (ignoring NA values)

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
     
     ## Loop through the input number of ids.
     for (intLoop in id) {
          
          ## read the actual file
          intLoopCounter <- intLoopCounter + 1
          matrixFileData <- read.csv(charFileName[intLoopCounter]) 
          matrixFileData
          
          ## verify the column names are what we expected
          colnames(matrixFileData)
          
          ## add current file data to total data matrix
          if (intLoop == id[1]) 
               matrixAllData <- matrixFileData
          else
               matrixAllData <- rbind(matrixAllData, matrixFileData)
          matrixAllData
     }
     
     ## display current matrix data
     matrixAllData

     
     ## reset to or original working directory here
     setwd(charSaveWD)
     
     ## return value here
     realMean <- mean(matrixAllData[,pollutant], na.rm=TRUE)
     realMean
}
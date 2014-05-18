corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations
     charSaveWD<-getwd()
     
     ## Let's use our complete function to identify the number of
     ## complete data lines within all datasets
     matrixCompleteData<-complete(directory)
     
     ## set working directory from input directory
     setwd(directory)
     
     ## complete observations within each dataset.
     asnumericOutput <- 0
     
     ## set filename extension since we know this a CSV file
     charFileExtension <- ".csv"
     charFileExtension
     
     ## initialize loop counter for use in accessing data files
     intOutputCount <- 0
     
     ## for each complete dataset let's calculate the correlation
     ## between nitrate and sulfate.
     for (intLoop in 1:332) {
          if (matrixCompleteData[intLoop,2] >= threshold) {
               ## convert numeric vector to string vector of 3 characters each
               charFileID <- sprintf("%03d", intLoop)
               charFileID
               
               ## combine FileID and FileExtension into Filename for read
               charFileName <- paste(charFileID, charFileExtension, sep="")
               charFileName
               
               matrixFileData <- read.csv(charFileName) 
               matrixFileData
               
               ## get the subset of completed co
               matrixNoNullCol2<-subset(matrixFileData, !is.na(matrixFileData[2]))
               matrixComplete<-subset(matrixNoNullCol2, !is.na(matrixNoNullCol2[3]))

               ## increment output data counter if number of completions
               ## is great enough to consider it relevant
               if (nrow(matrixComplete) >= threshold) { 
                    intOutputCount <- intOutputCount + 1
                    numericOutput[intOutputCount]<-
                         cor(matrixComplete[2],matrixComplete[3])
               }
          }
     }
     
     ## change back to our working directory
     setwd(charSaveWD)
     
     ## return our output data to the caller
     numericOutput
}
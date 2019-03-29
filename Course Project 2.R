#PART I
#directory C:/Users/User/specdata

PollutantMean<- function(directory, pollutant, id = 1:332) 
{
  
  #create a ist of files
  files_full<- list.files(directory, full.names = TRUE) 
  #Create an empty data frame
  dat<- data.frame()
  for (i in id)
    
  {
    #Add files to main data, enumerates cases by index "i" to see which directory/file 
    dat<- rbind(dat, read.csv(files_full[i])) 
  }
  
  #Compute for the Mean
  mean_data<- mean(dat[, pollutant], na.rm = TRUE)
  return(mean_data)
  
}

#>PollutantMean("directory","pollutant",1:332)




#PART II

CompleteCases<- function(directory, id = 1:332)
{
  
  #create a ist of files
  files_full<- list.files(directory, full.names= TRUE)
  #create an empty data frame
  dat<- data.frame()
  for (i in id)
    
  {
    # Read files
    temp <- read.csv(files_full[i])
    # "nobs" are sum of all complete cases, complete.cases() returns a logical vector indicating which cases are complete.
    nobs <-sum(complete.cases(temp))
    # Enumerates complete case by index
    dat<-rbind(dat, data.frame(i, nobs))    
  }
  
  #Column names for the data
  colnames(dat) <- c("id", "nobs")
  return(dat)
  
}

#>CompleteCases("directory",id)




#PART III

Correlation <- function(directory, threshold = 0)
{
  #List of all files in form of a vector
  files_full<- list.files(directory, full.names= TRUE)
  #Empty data set
  dat<- vector(mode = "numeric", length = 0)
  #for I in 1 up to the length(number) of all files in the directory
  for(i in 1:length(files_full))
  {
    
    # Read File
    tmp<- read.csv(files_full[i])
    #Calculate csum
    csum<- sum((!is.na(tmp$sulfate)) & (!is.na(tmp$nitrate)))
    #sum of all observed cases should be greater than threshold as per instructions
    if (csum> threshold) 
    {
      
      #Extract data of nitrate and sulfate and calculate correlation between them
      sul<- tmp[which(!is.na(tmp$sulfate)), ]
      nit <- sul[which(!is.na(sul$nitrate)), ]
      dat<- c(dat, cor(nit$sulfate, nit$nitrate))
    }
  }
  return(dat)
}

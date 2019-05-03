                    #Part I

setwd("C:/Users/User/Documents/New Folder")
getwd()
outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome_data)
 
 #Histogram of 30-day mortality rates for Heart Attack
outcome_data[, 11] <- as.numeric(outcome_data[, 11])
hist(outcome_data[, 11])




                    #Part II

best<- function(state, outcome){
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 
   if(!any(state == outcome1$State)){
    stop()
     }
  
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop()
  }
 
   outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
 
    else if (outcome == "heart failure") {
    colnum <- 17
  }
   
   else {
    colnum <- 23
  }
  
   min_row <- which(as.numeric(outcome2[ ,colnum]) == min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
  hospitals <- outcome2[min_row,2]
  hospitals <- sort(hospitals)
  
  return(hospitals[1])
}


                  

                    #Part III

rankhospital<- function(state, outcome, num = "best"){
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!any(state == outcome1$State)){
    stop(print("invalid state"))}
 
   else if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE){
    stop(print("invalid outcome"))
   }
  
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
 
   else if (outcome == "heart failure") {
    colnum <- 17
  }
 
   else {
    colnum <- 23
  }
  
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  outcome3 <- outcome2[order(outcome2[ ,colnum],outcome2[,2]), ]
  outcome3 <- outcome3[(!is.na(outcome3[ ,colnum])),]
 
   if(num == "best"){
    num <- 1
  }            
  
  else if (num == "worst"){
    num <- nrow(outcome3)
  }      
  
  return(outcome3[num,2])
}
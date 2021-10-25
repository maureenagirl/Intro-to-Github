#1st Activity: Pollution Monitoring Data#

###Item Number 1 ###

setwd("C:/Users/LENOVO/Documents/rprog_data_specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  means <- c()                                                #assign an empty vector to variable "means"
  
  for(monitor in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "") #print(path)
    monitor_data <- read.csv(path)                                                          #create a monitor_data that reads the path
    interested_data <- monitor_data[pollutant]                                              #assigns the pollutant as the interested data for which mean is calculated
    means <- c(means, interested_data[!is.na(interested_data)])                             #assigns the interested data to variable means
  }
  
  mean(means)                                                                               #Returns the mean of the interested data
}

##Sample code##

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)


----------------------------------------------
###Item Number 2 ###

complete <- function(directory, id = 1:332){
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  for(monitor in id){
    path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "") #print(path)
    monitor_data <- read.csv(path)                                                          #print(monitor_data)
    interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]                       #assigns the sulfate data as interested data
    interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]                 #assigns the nitrate data as interested data
    nobs <- nrow(interested_data)                                                           #assigns the number of row of interested data to variable nobs(number of complete cases)
    results <- rbind(results, data.frame(id=monitor, nobs=nobs))                            #results as data frame where 1st column is monitor id number and 2nd column as the no. of complete cases
  }
  results
}

##Sample code##

complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))

-----------------------------------------------------------------
###Item Number 3 ###

corr <- function(directory, threshold = 0){
  cor_results <- numeric(0)
  
  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
  
  if(nrow(complete_cases)>0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "") #print(path)
     
      monitor_data <- read.csv(path)                                                          #print(monitor_data)
     
      interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]                       #assigns the sulfate data as interested data
      interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]                 #assigns the nitrate data as interested data
      sulfate_data <- interested_data["sulfate"]                                          
      nitrate_data <- interested_data["nitrate"]
      cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))                          #solves for the correlation of 2 vectors
    }
  }
  cor_results                                                                                 #returns the result of the correlation
}

##Sample code##

cr<- corr("specdata", 150)
head(cr); summary(cr)

-------------------------------------------------------------------
#Item Number 4
#2nd Activty: Histogram of Hospital 30-day Mortality Rates from Heart attack

setwd("C:/Users/LENOVO/Documents/rprog_data_ProgHospData")
knitr::opts_chunk$set(echo = TRUE, results = "asis")

outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")               #reads the csv file 
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])                                                  #prints the 11th column from outcome dataset that prints the 30-day mortality rate for heart attack

hist(outcome[, 11])                                                                         #prints the histogram 

##sample code##
outcome[, 25] <- as.numeric(outcome[, 25]) 
hist(outcome[, 25]) 

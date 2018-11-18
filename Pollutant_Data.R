
getwd()
setwd("C:/Users/niede/Documents/Coursera_DataScience/specdata/")

pollutantmean <- function(directory, pollutant, id = 1:332){
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for(i in id){
    data <- read.csv(filelist[i])
    values <- c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}

pollutantmean("C:/Users/niede/Documents/Coursera_DataScience/specdata/", "sulfate")



complete <- function(directory, pollutant, id = 1:332){
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()

for(i in id){
  data <- read.csv(filelist[i])
  nobs <- c(nobs, sum(complete.cases(data)))
  }
data.frame(id, nobs)
}

complete("C:/Users/niede/Documents/Coursera_DataScience/specdata/")



corr <- function(directory, threshold = 0) {
  tcorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
  return (tcorrs)
}

corr("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 150)


# Week 2 Programming Quiz

pollutantmean("C:/Users/niede/Documents/Coursera_DataScience/specdata/", "sulfate", 1:10)

pollutantmean("C:/Users/niede/Documents/Coursera_DataScience/specdata/", "nitrate", 70:72)

pollutantmean("C:/Users/niede/Documents/Coursera_DataScience/specdata/", "sulfate", 34)

pollutantmean("C:/Users/niede/Documents/Coursera_DataScience/specdata/", "nitrate")

cc <- complete("C:/Users/niede/Documents/Coursera_DataScience/specdata/", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("C:/Users/niede/Documents/Coursera_DataScience/specdata/")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 2000)                
n <- length(cr)                
cr <- corr("C:/Users/niede/Documents/Coursera_DataScience/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

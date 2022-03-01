pollutantmean <- function(directory, pollutant, id=1:332) { 
  
  files_list <- list.files("~/Documents/R/Coursera/datasciencecoursera/specdata", full.names = TRUE) 
  df <- data.frame() #creates an empty data frame
  
  for (i in id) {
    df <- rbind(df, read.csv(files_list[i]))
  }
  
  df_subset <- df[,pollutant] 
  
  mean(df_subset, na.rm = TRUE) 
}



complete <- function(directory, id = 1:332) {  
  nobs <- numeric(length(id))
  
  files_list <- list.files("~/Documents/R/Coursera/datasciencecoursera/specdata", full.names = TRUE) 
  nobs <- numeric()
  
  for (i in id) { 
    data <- read.csv(files_list[i]) 
    
    complete_df <- complete.cases(data)
    
    nobs <- c(nobs, sum(complete_df))
  }
  data.frame(id, nobs)
}



corr <- function(directory, threshold = 0) {

  files_list <- list.files("~/Documents/R/Coursera/datasciencecoursera/specdata", full.names = TRUE) 
  correlation <- numeric()
  data <- data.frame()
  
  for (i in 1:332) {
    data <- read.csv(files_list[i]) 
    
    complete_df <- complete.cases(data)
    
    if (sum(complete_df) > threshold) {
      correlation <- 
        c(correlation, cor(x = data$sulfate, y = data$nitrate, use = "complete.obs"))
    }
    else{NULL}
  }
  correlation
}

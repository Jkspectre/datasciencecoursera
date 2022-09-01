pollutantmean <- function(directory, pollutant, id=1:332){
 filelist <- list.files(directory, full.names = TRUE)
 df <- data.frame()
 
 for (i in id){
   df <- rbind(df, read.csv(filelist[i]))
 }
 
 df_subset <- df[,pollutant]
 mean(df_subset, na.rm=TRUE)
}

library(tidyverse)
complete <- function(directory, id=1:332){
  filelist <- list.files(directory, full.names = TRUE)
  df1 <- data.frame()
 # colnames(df1) <- c('id', 'nobs')

  for(i in id){
    id_df <- read.csv(filelist[i]) %>% drop_na(nitrate, sulfate)
    df1 <- rbind(df1, data.frame('id'=i, 'nobs'=nrow(id_df)))
  }  
  df1
}

# corr <- function(directory, threshold = 0){
#     # requires lapply and data.table knowledge
# }

corr <- function(directory, threshold = 0) {
  
  # Make sure the R data.table package is loaded
  require(data.table)
  
  # Reading in all files and making a large data.table
  files_list <- lapply(file.path(directory, list.files(path = directory,pattern ="*.csv")), data.table::fread)
  
  # Bind all of the csv files together into one big file.
  dt <- rbindlist(files_list)
  
  # Only keep the completely observed cases
  dt <- dt[complete.cases(dt),]
  
  # Apply threshold and calculate correlations for those IDs whose nobs exceed the threshold 
  dt <- dt[, .(nobs = .N, corr = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
  
  return(dt[, corr])
}
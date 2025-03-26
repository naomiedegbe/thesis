# Data Formatting Functions

library(tidyverse)
library(ggplot2)
library(dplyr)

renameColumns <- function(df){
  # This function takes the first column and prepares it for the pivot table. Then it changes LTM - Last Twelve Months
  # to LTM. Finally it removes the exact date and just keeps the year of the Annual information. The function 
  # returns the data frame with the new columns. You may need to add the appl identifier somehow to categorize the data
  namesOfColumns <- colnames(df)
  namesOfColumns[1] <- "colNames"
  namesOfColumns[2] <- "LTM"
  namesOfColumns[3:length(namesOfColumns)] <- sapply(namesOfColumns[3:length(namesOfColumns)], function(x) substr(x, 1, 4))
  colnames(df) <- namesOfColumns
  return(df)
}


transposeData <- function(df, stockName){
  # Input is data frame received from renameCols function
  # This function changes the orientation of the data frame to account for each year as row entries
  # Data from this function should be suitable for EDA. Additionally, a stock column (input 2)
  # is added to prevent any issues that may arise from merging the data frames.
  
  
  # Step 1: Obtain column names from df
  newNames <- df$colNames
  # Step 2: Transpose every column but the column names
  transposeDF <- t(df[,-1])
  # Step 3: Assign new column names
  colnames(transposeDF) <- newNames
  # Step 4: Convert the transposed data back to a Data frame for better readability
  df <- as.data.frame(transposeDF)
  # Step 5: Make original column entries row 
  df$Year <- rownames(df)
  # Step 6: Reorder columns to place the new column first
  df <- df[, c("Year", names(df)[names(df) != "Year"])]
  df$stock <- stockName
  return(df)
  
}

wrangleData <- function(df, stockName){
  # This function does utilizes the renameColumns and transpose
  # Data functions to return 1 data frame fixed in one function.
  step1 <- renameColumns(df)
  step2 <- transposeData(step1, stockName)
  return(step2)
}

mergeFinanceData <- function(df1, df2, stockName){
  # this takes the two data frames and merges them
  # with the formatting above
  df1_update <- wrangleData(df1, stockName)
  df2_update <- wrangleData(df2, stockName)
  df <- merge(df1_update, df2_update, by=c("Year","stock"), all = TRUE)
  return(df)
}

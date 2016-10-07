#' OLATTestAnalyzeR
#'   
#' @description Formats and analysis OLAT - Online Learning and Training Single Choice Tests
#'   
#' @details 
#'   Features:
# 
#'   - Formatted output with ...
#'     - letters as column names
#'     - marks (*) the correct answers
#'     - Sum of answer columns
#'     - Mean Test score
#'     - Proportion of correct answers (p = difficulty)
#'     - Proportion of incorrect answers
#'     - Point-biserial correlation coefficient for each question (r = selectivity) 
#'  
#' @param file xls or csv file
#' @param sep tab ('\t') as default delimiter
#' @param encoding 'ISO-8859-1' as default
#'  
#' @author Benjamin Gerwoll-Ronca
#' @version 0.0.9001
#' @license GPL-3
#' @keywords Item analysis
#'
#' @examples
#'  test <- analyser('OLAT SCQs.xls', sep = '\t') # reading xls-file with analyzer()
#'  
#' @export

analyzer <- function(file, sep=';', encoding='ISO-8859-1'){
  
  # setting working directory to path
  # setwd(path)
  
  # load OLAT export of the exam
  csv <- read.csv(file, header = FALSE, sep = sep, encoding = encoding, stringsAsFactors = FALSE)
  
  ##### Regex patterns #####
  
  #path_patt <- '[-[:alnum:][:space:]%_]+\\.(csv|xls|txt)$'
  num_answers <- '\\d+_(R|C)\\d+'
  scores <- '\\d+_Score'
  
  ##### Helper Functions #####
  
  split_dataframe <- function(data){
    for (i in 2:nrow(data)){      # start with row 2 (header)
      if (data[i,1] == ''){
        end <- i - 1
        break
      }
    }
    return(data[1:end,])
  }
  
  convert_datatypes <- function(df){
    num_answers <- '\\d+_(R|C)\\d+'
    scores <- '\\d+_Score'
    numeric_cols <- c('Sequence number', 'Test score', 'Total time (s)')
    boolean_col <- 'Passed'
    date_col <- 'Date'
    for (col in colnames(df)){
      if (col %in% numeric_cols | grepl(num_answers, col) | grepl(scores, col)){
        df[,col] <- as.numeric(df[,col])
      }
      if (col == boolean_col){
        df[,col] <- as.logical(df[,col])
      }
      if (col == date_col){
        df[,col] <- as.POSIXct(df[,col], format = "%Y-%m-%dT%H:%M:%S")
      }
    }
    return(df)
  }
  
  group_mean <- function(df, col, x, n){
    tmp_score <- numeric()
    for (i in 1:n){
      if (df[i, col] == x){
        tmp_score <- c(tmp_score, df[i, 'Test score'] - x)
      }
    }
    group_mean <- mean(tmp_score)
    if (is.na(group_mean)){
      return(0)
    }
    else {
      return(group_mean)
    }
  }
  
  point_biserial_corr <- function(df, col, n){
    tmp_score = numeric()
    for (i in 1:n){
      if (df[i, col] == 1){
        tmp_score[i] <- df[i, 'Test score'] - 1
      }
      else {
        tmp_score[i] <- df[i, 'Test score']
      }
    }
    std <- sd(tmp_score)
    n_1 <- sum(df[1:n,col] == 1)
    n_0 <- n - n_1
    rpb <- ((df['p-mean', col] - df['q-mean', col]) / std) * sqrt((n_1 * n_0) / n**2)
    return(rpb)
  }
  
  get_path <- function(file_path){
    path_patt <- '[-[:alnum:][:space:]%_]+\\.(csv|xls|txt)$'
    match_len <- nchar(regmatches(file_path, regexpr(path_patt, file_path)))
    stop <- nchar(file_path) - match_len
    return(substring(file_path, 1, stop))
  }
  
  ##### Setting working directory ####
  path <- get_path(file)
  setwd(path)
  
  ##### Preparing the dataframe #####
  
  # cutting off the legend
  df <- split_dataframe(csv)
  
  # defining question and header row
  # questions <- df[1,]
  #header <- df[2,]
  assign("questions", df[1,], envir = .GlobalEnv)
  assign("header", df[2,], envir = .GlobalEnv)
  
  # removing first two rows
  df <- df[-c(1,2),]
  
  # adding header to df
  colnames(df) <- header
  n <- nrow(df)
  # converting datatypes
  df <- convert_datatypes(df)
  
  ##### calculating column sums, difficulty, and Point-biserial correlation coefficient #####
  
  # adding empty rows
  df['Sum',] <- NA
  df['p',] <- NA
  df['q',] <- NA
  df['p-mean',] <- NA
  df['q-mean',] <- NA
  df['r',] <- NA
  
  # adding row description in col 'Sequence number'
  df['Sum','Sequence number'] <- 'Sum'
  df['p','Sequence number'] <- 'p'
  df['q','Sequence number'] <- 'q'
  df['p-mean','Sequence number'] <- 'p mean'
  df['q-mean','Sequence number'] <- 'q mean'
  df['r','Sequence number'] <- 'Point-biserial correlation coefficient'
  
  # looping over columns
  score_cols <- character()
  
  for (col in colnames(df)){
    if (grepl(num_answers, col)){
      # summing answer columns
      df['Sum', col] <- sum(df[1:n,col], na.rm = TRUE) # sum of answer columns
    }
    if (col == 'Test score'){
      # mean of Test score
      df['Sum', col] <- mean(df[1:n,col], na.rm = TRUE)
    }
    if (grepl(scores, col)){
      score_cols <- c(score_cols, col)
      # difficulty and rpb
      df['p', col] <- mean(df[1:n,col], na.rm = TRUE)  # mean of score columns
      df['q', col] <- 1 - df['p', col]
      df['p-mean', col] <- group_mean(df, col, 1, n)
      df['q-mean', col] <- group_mean(df, col, 0, n)
      df['r', col] <- point_biserial_corr(df, col, n)
    }
  }
  return(df)
}
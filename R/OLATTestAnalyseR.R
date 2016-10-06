#
#'   OLATTestAnalyzeR
#'   
#'   @author Benjamin Gerwoll-Ronca
#'   @version 0.0.9001
#'   @license GPL-3
#   
#'   => FOR SINGLE CHOICE QUESTIONS ONLY !
#
#'   Features:
# 
#'   - Formated output with ...
#'     - Sum of answer columns
#'     - Mean Test score
#'     - Proportion of correct answers (p = difficulty)
#'     - Proportion of incorrect answers
#'     - Point-biserial correlation coefficient for each question (r = selectivity) 
#'   - Scatter plot Difficulty vs. Selectivity
#'   - Bar chart Distribution of Test score
#'   - Bar chart Distribution of completed exams over examination period
#'  @export
#'  @examples
#'  test <- analyser('OLAT SCQs.xls', sep = '\t') # reading xls-file with analyzer()
#'  plot_discrimination(test)                     # scatter plot of selectivity vs. difficulty
#'  plot_score(test)                              # bar chart distribution of test scores
#'  analyser.write(test, 'OLAT SCQs formatted.xls', keys = 'question_keys.csv') # writing formatted test into xls-file


# main function
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

##### Plotting #####

# Selectivity vs. difficulty
plot_discrimination <- function(df){
  score_patt <- '\\d+_Score'
  scores <- vector()
  cols <- colnames(df)
  j <- 1
  for (i in 1:length(cols)){
    if (grepl(score_patt, cols[i])){
      scores[j] <- cols[i]
      j <- j + 1
    }
  }
  score_cols <- subset(df[, scores])
  difficulty <- as.vector(as.matrix(score_cols['p',]))
  selectivity <- as.vector(as.matrix(score_cols['r',]))
  plot(difficulty, selectivity, xlim=rev(c(0.0,1.0)), main = 'Selectivity vs. difficulty', ylab = 'Selectivity (rpb)', xlab = 'Difficulty (p)')
  abline(h=0, col='blue')
  return(list(selectivity, difficulty))
}

# Distribution of the test score
plot_score <- function(df, max_score=25){
  n <- length(!is.na(df[, 'Name']))
  test_score <- as.matrix(table(factor(df[1:n,'Test score'], levels=1:max_score)))
  cumfreq <- numeric()
  for(i in 1:max_score){
    if(i > 1){
      cumfreq[i] <- test_score[i] / sum(test_score) * 100 + cumfreq[i-1]
    }
    else{
      cumfreq[i] <- test_score[i] / sum(test_score) * 100
    }
  }
  median_score <- median(df[1:n,'Test score'])
  par(mar=c(5, 4, 4, 6) + 0.1)
  ymax <- max(test_score) + 5
  barplot(test_score[,1], ylim = c(0,ymax),main = 'Distribution of Test score', xlab = 'Score', ylab = 'Frequency', col = 'chocolate')
  par(new = T)
  plot(cumfreq, axes=FALSE, type = 'l', lwd=2, col = 'blue', ylab=NA, xlab=NA)
  axis(4, at = seq(0,100,20))
  abline(v=median_score, col='green', lwd=2)
}


##### Preparing for output as xls file #####

analyzer.write <- function(df, file = 'OLAT_Test_Analysis.xls', keys = NULL, sep = '\t', na = '', row.names = FALSE){
  #### Helper functions ####
  put_letters <- function(item, keys=NULL){
    if (!is.null(keys)){
      k <- read_keys(keys)
    }
    else {
      k <- keys
    }
    question_patt <- '^\\d+'
    answer_patt <- '\\d+$'
    q <- regmatches(item, regexpr(question_patt, item))
    d <- regmatches(item, regexpr(answer_patt, item))
    l <- letters[as.integer(d)]
    if (!is.null(k)){
      if (k[as.integer(q)] == l){
        l <- paste(l, '*', sep = '')
      }
    }
    return(l)
  }
  
  read_keys <- function(csv, sep=';'){
    keys <- read.table(csv, sep=sep, stringsAsFactors = FALSE)
    return(keys$V2)
  }

  
  # resetting the column index and adding question and header rows to df
  colnames(df) <- questions[1,]
  colnames(header) <- questions[1,]
  output_csv <- rbind(header, df)
  
  # enumerate questions
  c_names <- colnames(output_csv)
  j <- 1
  for (i in 1:length(c_names)){
    if (c_names[i] != ''){
      c_names[i] <- paste(paste(toString(j), '.', sep = ''), c_names[i], sep = ' ')
      j <- j + 1
    }
  }
  colnames(output_csv) <- c_names
  
  # replacing answer labels with letters
  num_answers <- '\\d+_(R|C)\\d+'
  for (i in 1:length(output_csv[1,])){
    if (grepl(num_answers, output_csv[1,][i])){
      if (is.null(keys)){
        output_csv[1,][i] <- put_letters(output_csv[1,][i])
      }
      else {
        output_csv[1,][i] <- put_letters(output_csv[1,][i], keys)
      }
    }
    else {
      next
    }
  }
  
  # saving dataframe as xls file 
  write.table(output_csv, file = file, sep = sep, na = na, row.names = FALSE)
}

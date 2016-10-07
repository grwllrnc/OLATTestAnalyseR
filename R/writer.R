#' OLATTestAnalyzeR
#'   
#' @description Writes formatted output of the analyser() function to a xls or csv file
#'  
#' @param df output of the analyzer() function
#' @param file (path and) name of the output file
#' @param keys csv file (2 column) with key of the correct answer
#' @param sep tab ('\t') as default delimiter
#' 
#' @author Benjamin Gerwoll-Ronca
#' @keywords {Item analysis} OLAT
#'
#' @examples
#' analyser.write(test, 'OLAT SCQs formatted.xls', keys = 'question_keys.csv') # writing formatted test into xls-file
#'
#' @export

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

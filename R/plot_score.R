#' OLATTestAnalyzeR
#'   
#' @description Bar chart Distribution of Test score
#'  
#' @param df output of analyzer() function
#'  
#' @author Benjamin Gerwoll-Ronca
#' @version 0.0.9003
#' @license GPL-3
#' @keywords Item analysis
#'
#' @examples
#'  plot_score(test, max_score = 25)  # bar chart distribution of test scores
#'  
#' @export

# Distribution of the test score
plot_score <- function(df, max_score=25){
  if ('Test score' %in% colnames(df)){
    t_score <- 'Test score'
  }
  if ('Test Punkte' %in% colnames(df)) {
    score_col <- 'Test Punkte'
  }
  if ('Points test' %in% colnames(df)){
    score_col <- 'Points test'
  }
  n <- length(!is.na(df[, 3]))
  test_score <- as.matrix(table(factor(df[1:n, score_col], levels=1:max_score)))
  cumfreq <- numeric()
  for(i in 1:max_score){
    if(i > 1){
      cumfreq[i] <- test_score[i] / sum(test_score) * 100 + cumfreq[i-1]
    }
    else{
      cumfreq[i] <- test_score[i] / sum(test_score) * 100
    }
  }
  median_score <- median(df[1:n, score_col])
  par(mar=c(5, 4, 4, 6) + 0.1)
  ymax <- max(test_score) + 5
  barplot(test_score[,1], ylim = c(0,ymax),main = 'Distribution of Test score', xlab = 'Score', ylab = 'Frequency', col = 'chocolate')
  par(new = TRUE)
  plot(cumfreq, axes=FALSE, type = 'l', lwd=2, col = 'blue', ylab=NA, xlab=NA)
  axis(4, at = seq(0,100,20))
  abline(v=median_score, col='green', lwd=2)
}
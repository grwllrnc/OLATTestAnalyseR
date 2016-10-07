#' OLATTestAnalyzeR
#'   
#' @description Bar chart Distribution of Test score
#'  
#' @param df output of analyzer() function
#'  
#' @author Benjamin Gerwoll-Ronca
#' @version 0.0.9001
#' @license GPL-3
#' @keywords Item analysis
#'
#' @examples
#'  plot_score(test)  # bar chart distribution of test scores
#'  
#' @export

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
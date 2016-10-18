#' OLATTestAnalyzeR
#'   
#' @description Scatter plot Difficulty vs. Selectivity
#'  
#' @param df output of analyzer() function
#'  
#' @author Benjamin Gerwoll-Ronca
#' @version 0.0.9003
#' @license GPL-3
#' @keywords Item analysis
#'
#' @examples
#'  plot_discrimination(test) # scatter plot of selectivity vs. difficulty
#'  
#' @export

# Selectivity vs. difficulty
plot_discrimination <- function(df){
  score_patt <- '\\d+_(Score|Pkt|Pts)'
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
}
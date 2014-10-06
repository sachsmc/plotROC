#' Calculate the ROC curve
#' 
#' Calculate ROC curve given labels and predictions
#' 
#' @param M continuous marker values or predictions of class labels
#' @param D class labels
#' 
#' @return Data frame containing cutoffs, and estimated true and false positive fractions
#' 
#' @export
#' 
#' @examples
#' 
#' D.ex <- rbinom(100, 1, .5)
#' calculate_roc(rnorm(100, mean = D.ex), D.ex)

calculate_roc <- function(M, D){
  
  if(length(unique(D)) != 2) stop("Only labels with 2 classes supported")
  if(sum(c(is.na(M), is.na(D))) > 0) stop("No missing data allowed")
  
  c <- sort(M)
  TPF <- sapply(c, function(x) mean(M[D == 1] > x))
  FPF <- sapply(c, function(x) mean(M[D == 0] > x))
  
  df <- data.frame(cbind(c, TPF, FPF))
  class(df) <- append(class(df), "rocdata")
  df
  
}


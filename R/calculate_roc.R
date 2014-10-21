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
  df
  
}


#' Calculate the ROC curve for multiple biomarkers
#' 
#' Calculate ROC curves given labels and predictions. Designed to work with the \code{multi_ggroc} function, this takes a dataframe and computes the ROC curve for a given list of markers. 
#' 
#' @param data Data frame containing at least 1 marker and the common class labels
#' @param M_string vector of marker column names
#' @param D_string class label column name
#' 
#' @return List of data frame containing cutoffs, and estimated true and false positive fractions
#' 
#' @export
#' 
#' @examples
#' 
#' D.ex <- rbinom(100, 1, .5)
#' fakedata <- data.frame(M1 = rnorm(100, mean = D.ex), M2 = rnorm(100, mean = D.ex, sd = .4), D = D.ex)
#' calculate_multi_roc(fakedata, c("M1", "M2"), "D")

calculate_multi_roc <- function(data, M_string, D_string){
  
  out_list <- vector("list", length = length(M_string))

  D <- data[D_string][,1]
  if(length(unique(D)) != 2) stop("Only labels with 2 classes supported")
  
  for(i in 1:length(out_list)){
        
      M <- data[M_string[i]][,1]

      if(sum(c(is.na(M), is.na(D))) > 0) stop("No missing data allowed")
      
      c <- sort(M)
      TPF <- sapply(c, function(x) mean(M[D == 1] > x))
      FPF <- sapply(c, function(x) mean(M[D == 0] > x))
      
      df <- data.frame(cbind(c, TPF, FPF))
      out_list[[i]] <- df
  }
  
  out_list
  
}

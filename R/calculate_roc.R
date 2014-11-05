#' Calculate the ROC curve
#' 
#' Calculate ROC curve given labels and predictions
#' 
#' @param M continuous marker values or predictions of class labels
#' @param D class labels
#' @param CI Logical, if true, will calculate exact joint confidence regions for
#'   the TPF and FPF
#' @param alpha Confidence level, ignored if \code{CI = FALSE}
#'   
#' @return Data frame containing cutoffs, and estimated true and false positive
#'   fractions
#'   
#' @details Confidence intervals for TPF and FPF are calculated using the exact
#'   method of Clopper and Pearson (1934) each at the level \code{1 - sqrt(1 -
#'   alpha)}. Based on result 2.4 from Pepe (2003), the cross-product of these
#'   intervals yields a 1 - alpha % confidence region for (FPF, TPF).
#'   
#' @export
#' 
#' @examples
#' 
#' D.ex <- rbinom(100, 1, .5)
#' calculate_roc(rnorm(100, mean = D.ex), D.ex)
#' calculate_roc(rnorm(100, mean = D.ex), D.ex, CI = TRUE)

calculate_roc <- function(M, D, CI = FALSE, alpha = .05){
  
  if(length(unique(D)) != 2) stop("Only labels with 2 classes supported")
  if(sum(c(is.na(M), is.na(D))) > 0) stop("No missing data allowed")
  
  c <- sort(M)
  TPF <- sapply(c, function(x) mean(M[D == 1] > x))
  FPF <- sapply(c, function(x) mean(M[D == 0] > x))
  
  df <- data.frame(cbind(c, TPF, FPF))
  
  if(CI){
    
    stopifnot(is.finite(alpha) && alpha < 1 && alpha > 0)
    ## calculate binomial confidence regions using Clopper and Pearson method
    alpha.star <- 1 - sqrt(1 - alpha)
    n0 <- sum(D == 0)
    n1 <- sum(D == 1)
    M0 <- M[D == 0]
    M1 <- M[D == 1]
    
    ci_res <- sapply(c, function(x){ 
      
      
      FP.L <- qbeta(alpha.star, sum(M0 > x), n0 - sum(M0 > x) + 1)
      FP.U <- qbeta(1  - alpha.star, sum(M0 > x) + 1, n0 - sum(M0 > x))
      
      TP.L <- qbeta(alpha.star, sum(M1 > x), n1 - sum(M1 > x) + 1)
      TP.U <- qbeta(1  - alpha.star, sum(M1 > x) + 1, n1 - sum(M1 > x))
      
      c(FP.L, FP.U, TP.L, TP.U)
      
      })
    
    ci_res2 <- as.data.frame(t(ci_res))
    colnames(ci_res2) <- c("FP.L", "FP.U", "TP.L", "TP.U")
    df <- cbind(df, ci_res2)
  }
  df
  
}


#' Calculate the ROC curve for multiple biomarkers
#' 
#' Calculate ROC curves given labels and predictions. Designed to work with the
#' \code{multi_ggroc} function, this takes a dataframe and computes the ROC
#' curve for a given list of markers.
#' 
#' @param data Data frame containing at least 1 marker and the common class
#'   labels
#' @param M_string vector of marker column names
#' @param D_string class label column name
#'   
#' @return List of data frame containing cutoffs, and estimated true and false
#'   positive fractions
#'   
#' @export
#' 
#' @examples
#' 
#' D.ex <- rbinom(100, 1, .5)
#' fakedata <- data.frame(M1 = rnorm(100, mean = D.ex), 
#'    M2 = rnorm(100, mean = D.ex, sd = .4), D = D.ex)
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


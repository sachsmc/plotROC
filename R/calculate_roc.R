#' Calculate the Empirical ROC curve
#' 
#' Deprecated, use \link{geom_roc} instead
#' 
#' @param M continuous marker values or predictions of class labels
#' @param D class labels, must be coded as 0 and 1. If not numeric with 0/1,
#'   then plotROC assumes the first level in sort order is healthy status, with
#'   a warning.
#' @param ci Logical, if true, will calculate exact joint confidence regions for
#'   the TPF and FPF
#' @param alpha Confidence level, ignored if \code{ci = FALSE}
#'   
#' @return A dataframe containing cutoffs, estimated true and false positive 
#'   fractions, and confidence intervals if \code{ci = TRUE}.
#'   
#' @details Confidence intervals for TPF and FPF are calculated using the exact 
#'   method of Clopper and Pearson (1934) each at the level \code{1 - sqrt(1 - 
#'   alpha)}. Based on result 2.4 from Pepe (2003), the cross-product of these 
#'   intervals yields a 1 - alpha % confidence region for (FPF, TPF).
#'   
#' @export
#' 


calculate_roc <- function(M, D, ci = FALSE, alpha = .05){
  
  if(sum(c(is.na(M), is.na(D))) > 0) stop("No missing data allowed")
  
  D <- verify_d(D)
  T.order <- order(M, decreasing=TRUE)
  TTT <- M[T.order]
  TPF <- cumsum(D[T.order] == 1)
  FPF <- cumsum(D[T.order] == 0)
  
  ## remove fp & tp for duplicated predictions
  ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
  
  dups <- rev(duplicated(rev(TTT)))
  tp <- c(0, TPF[!dups])/sum(D == 1)
  fp <- c(0, FPF[!dups])/sum(D == 0)
  cutoffs <- c(Inf, TTT[!dups])
  
  df <- data.frame(FPF = fp, TPF = tp, c = cutoffs)
  
  
  if(ci){
    
    stopifnot(is.finite(alpha) && alpha < 1 && alpha > 0)
    ## calculate binomial confidence regions using Clopper and Pearson method
    alpha.star <- 1 - sqrt(1 - alpha)
    n0 <- sum(D == 0)
    n1 <- sum(D == 1)
    M0 <- M[D == 0]
    M1 <- M[D == 1]
    
    ci_res <- sapply(c, function(x){ 
      
      
      FP.L <- stats::qbeta(alpha.star, sum(M0 > x), n0 - sum(M0 > x) + 1)
      FP.U <- stats::qbeta(1  - alpha.star, sum(M0 > x) + 1, n0 - sum(M0 > x))
      
      TP.L <- stats::qbeta(alpha.star, sum(M1 > x), n1 - sum(M1 > x) + 1)
      TP.U <- stats::qbeta(1  - alpha.star, sum(M1 > x) + 1, n1 - sum(M1 > x))
      
      c(FP.L, FP.U, TP.L, TP.U)
      
      })
    
    ci_res2 <- as.data.frame(t(ci_res))
    colnames(ci_res2) <- c("FP.L", "FP.U", "TP.L", "TP.U")
    df <- cbind(df, ci_res2)
  }
  df
  
}


#' Calculate the Empirical ROC curves for multiple biomarkers
#' 
#' Deprecated, use \link{geom_roc} instead
#' 
#' @param data data frame containing at least 1 marker and the common class
#'   labels, coded as 0 and 1
#' @param M_string vector of marker column names
#' @param D_string class label column name
#'   
#' @return List of data frames containing cutoffs, and estimated true and false
#'   positive fractions
#'   
#' @export
#' 

calculate_multi_roc <- function(data, M_string, D_string){
  
  out_list <- vector("list", length = length(M_string))

  D <- verify_d(data[D_string][,1])
  
  for(i in 1:length(out_list)){
        
      M <- data[M_string[i]][,1]

      if(sum(c(is.na(M), is.na(D))) > 0) stop("No missing data allowed")
      
      T.order <- order(M, decreasing=TRUE)
      TTT <- M[T.order]
      TPF <- cumsum(D[T.order] == 1)
      FPF <- cumsum(D[T.order] == 0)
      
      ## remove fp & tp for duplicated predictions
      ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
      
      dups <- rev(duplicated(rev(TTT)))
      tp <- c(0, TPF[!dups])/sum(D == 1)
      fp <- c(0, FPF[!dups])/sum(D == 0)
      cutoffs <- c(Inf, TTT[!dups])
      
      df <- data.frame(FPF = fp, TPF = tp, c = cutoffs)
      
      
      out_list[[i]] <- df
  }
  
  out_list
  
}


#' Check that D is suitable for using as binary disease status
#' 
#' Checks for two classes and gives a warning message indicating which level is assumed to be 0/1
#' 
#' @param D Vector that will be checked for 2-class labels
#' 
#' @keywords Internal
#' 
verify_d <- function(D){

  if(length(levels(as.factor(D))) > 2) stop("Only labels with 2 classes supported")
  
  slev <- sort(levels(as.factor(D)))
  if(slev[1] == 0 & slev[2] == 1) return(D)
  
  warning(paste0("D not labeled 0/1, assuming ", slev[1], " = 0 and ", slev[2], " = 1!"))
  
  zero1 <- c(0, 1)
  names(zero1) <- slev
  
  zero1[D]
  
}

#' Transform biomarkers stored as wide to long
#' 
#' Multple biomarkers measured on the same subjects are often stored as multiple columns in a data frame. This is a convenience function that transforms the data into long format, suitable for use with ggplot and \link{geom_roc}
#' 
#' @param data Data frame containing disease status and biomarkers stored in columns
#' @param d Column containing binary disease status. Can be a column name or index
#' @param m Vector of column names or indices identifying biomarkers
#' @param names Optional vector of names to assign to the biomarkers. If NULL, names will be taken from the column names
#' 
#' @return A data frame in long format with three columns: D = binary disease status, M = biomarker value, and name = biomarker name
#' 
#' @export
#' @examples 
#' D.ex <- rbinom(50, 1, .5)
#' widedata <- data.frame(D = D.ex, M1 = rnorm(50, mean = D.ex, sd = 1), 
#'    M2 = rnorm(50, mean = D.ex, sd = .5))
#' longdata <- melt_roc(widedata, "D", c("M1", "M2"))
#' ggplot(longdata, aes(d = D, m = M, color = name)) + geom_roc()
#' 
melt_roc <- function(data, d, m, names = NULL){
  
  if(is.null(names)){
    
    names <- colnames(data[, m])
    
  }
  
  data.frame(D = rep(data[, d], length(m)), 
             M = do.call(c, data[, m]), 
             name = rep(names, each = nrow(data)), stringsAsFactors = FALSE)
  
}


is.discrete <- function(x) {
   is.factor(x) || is.character(x) || is.logical(x)
}


#' Calculate the Area under the ROC curve
#' 
#' Given a ggplot object with a GeomRoc layer, computes the area under the ROC curve for each group
#' 
#' @param ggroc A ggplot object that contains a GeomRoc layer
#' 
#' @export
#' @examples 
#' D.ex <- rbinom(50, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex),
#'                      M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)),
#'                      Z = c(rep("A", 50), rep("B", 50)))
#'
#' ggroc <- ggplot(rocdata, aes(m = M, d = D)) + geom_roc()
#' calc_auc(ggroc)
#' ggroc2 <- ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc()
#' calc_auc(ggroc2)

calc_auc <- function(ggroc){
  
  lays <- sapply(ggroc$layers, function(g) class(g$geom)[1])
  stopifnot("GeomRoc" %in% lays)
  
  l1 <- ggplot_build(ggroc)$data[[1]]
  
  comp_auc <- function(df){
    
    auc <- 0
    for (i in 2:length(df$x)) {
      auc <- auc + 0.5 * (df$x[i] - df$x[i-1]) * (df$y[i] + df$y[i-1])
    }
    return(data.frame(AUC = auc))
  }
  
  plyr::ddply(l1, ~ PANEL + group, comp_auc)
  
}

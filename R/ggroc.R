#' Plot an ROC curve
#' 
#' Given a result computed by \link{calculate_roc}, plot the curve using ggplot and sensible defaults
#' 
#' @param rocdata Data frame as returned by calculate_roc
#' @param fpf_string Column name identifying false positive fraction
#' @param tpf_string Column name identifying true positive fraction
#' 
#' @export
#' 
#' @return A ggplot object
#' 

ggroc <- function(rocdata, fpf_string = "FPF", tpf_string = "TPF"){
  
  stopifnot(fpf_string %in% colnames(rocdata))
  stopifnot(tpf_string %in% colnames(rocdata))
  
  min_br <-  c(seq(0, .1, by = .01), seq(.9, 1, by = .01))
  br <- c(0, .1, .25, .5, .75, .9, 1)
  
  p1 <- ggplot2::ggplot(rocdata, ggplot2::aes_string(x = fpf_string, y = tpf_string))  + ggplot2::geom_point(color = "red", alpha = 0) +
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 2) + 
    ggplot2::scale_x_continuous("False positive fraction", minor_breaks = min_br, breaks = br) + 
    ggplot2::scale_y_continuous("True positive fraction", minor_breaks = min_br, breaks = br) + ggplot2::geom_path() 
    
  p1
    
  
}

#' Plot multiple ROC curves
#' 
#' Given a list of results computed by \link{calculate_roc}, plot the curve using ggplot and sensible defaults
#' 
#' @param datalist List of data frames as returned by calculate_roc
#' @param fpf_string Column names identifying false positive fraction
#' @param tpf_string Column names identifying true positive fraction
#' @param lty Line types to distinguish curves
#' 
#' @export
#' 
#' @return A ggplot object
#' 

multi_ggroc <- function(datalist, fpf_string = rep("FPF", length(datalist)), tpf_string = rep("TPF", length(datalist)), 
                        lty = 1:length(datalist)){
  
  stopifnot(all(sapply(1:length(datalist), function(i) fpf_string[i] %in% colnames(datalist[[i]]))))
  stopifnot(all(sapply(1:length(datalist), function(i) tpf_string[i] %in% colnames(datalist[[i]]))))
  
  min_br <-  c(seq(0, .1, by = .01), seq(.9, 1, by = .01))
  br <- c(0, .1, .25, .5, .75, .9, 1)
  
  p1 <- ggplot2::ggplot(datalist[[1]], ggplot2::aes_string(x = fpf_string[1], y = tpf_string[1]))  + ggplot2::geom_point(color = "red", alpha = 0) +
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, color = "white") + 
    ggplot2::scale_x_continuous("False positive fraction", minor_breaks = min_br, breaks = br) + 
    ggplot2::scale_y_continuous("True positive fraction", minor_breaks = min_br, breaks = br) + ggplot2::geom_path(lty = lty[1]) 
  
  for(i in 2:length(datalist)){
    
    p1 <- p1 + ggplot2::geom_path(data = datalist[[i]], aes_string(x = fpf_string[i], y = tpf_string[i]), lty = lty[i]) + 
      ggplot2::geom_point(data = datalist[[i]], aes_string(x = fpf_string[i], y = tpf_string[i]), color = "red", alpha = 0)
    
  }
  
  p1
  
}



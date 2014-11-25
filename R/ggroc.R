#' Plot an ROC curve
#' 
#' Given a data frame or list of data frames as computed by \link{calculate_roc}
#' plot the curve using ggplot and sensible
#' defaults. 
#' 
#' @param rocdata Data frame containing true and false positive fractions, and cutoff values
#' @param fpf_string Column name identifying false positive fraction column
#' @param tpf_string Column name identifying true positive fraction column
#' @param ci Logical, if TRUE will create invisible confidence regions for use
#'   in the interactive plot
#' @param label Optional direct label for the ROC curve
#' @param label.adj.x Adjustment for the horizontal positioning of the label
#' @param label.adj.y Adjustment for the vertical positioning of the label
#' @param label.angle Adjustment for angle of label
#'   
#' @export
#' 
#' @return A ggplot object
#'   

ggroc <- function(rocdata, fpf_string = "FPF", tpf_string = "TPF", ci = FALSE,
                  label = NULL, label.adj.x = 0, label.adj.y = 0, label.angle = 45){
  
  stopifnot(fpf_string %in% colnames(rocdata))
  stopifnot(tpf_string %in% colnames(rocdata))
  
  min_br <-  c(seq(0, .1, by = .01), seq(.9, 1, by = .01))
  br <- c(0, .1, .25, .5, .75, .9, 1)
  
  p1 <- ggplot2::ggplot(rocdata, ggplot2::aes_string(x = fpf_string, y = tpf_string))  + ggplot2::geom_point(color = "red", alpha = 0) +
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, color = "white") + 
    ggplot2::scale_x_continuous("False positive fraction", minor_breaks = min_br, breaks = br) + 
    ggplot2::scale_y_continuous("True positive fraction", minor_breaks = min_br, breaks = br) + ggplot2::geom_path() 
    
  if(!is.null(label)){
    
    xy <- rocdata[rocdata$TPF + rocdata$FPF < 1, c(fpf_string, tpf_string)][1,]
    X <- xy[1] + label.adj.x + .05
    Y <- xy[2] - .05 + label.adj.y
    p1 <- p1 + ggplot2::geom_text(data = data.frame(FPF = X, TPF = Y, label = label), 
                                  ggplot2::aes_string(x = "FPF", y  = "TPF", label = "label"), angle = label.angle)
    
  }
  
  if(ci){
    
    p1 <- p1 + ggplot2::geom_rect(ggplot2::aes_string(xmin = "FP.L", xmax = "FP.U", ymin = "TP.L", ymax = "TP.U"), alpha = 0)
    
  }
  
  p1
    
  
}

#' Plot multiple ROC curves
#' 
#' Given a list of results computed by \link{calculate_roc}, plot the curve using ggplot and sensible defaults
#' 
#' @param datalist List of data frames each containing true and false positive fractions and cutoffs
#' @param fpf_string Column names identifying false positive fraction
#' @param tpf_string Column names identifying true positive fraction
#' @param lty Line types to distinguish curves
#' @param label Optional vector of direct labels for the ROC curve, same length as \code{datalist}
#' @param label.adj.x Adjustment for the positioning of the label, same length as \code{datalist}
#' @param label.adj.y Adjustment for the positioning of the label, same length as \code{datalist}
#' @param label.angle Adjustment for angle of label, same length as \code{datalist}
#' 
#' @export
#' 
#' @return A ggplot object
#' 

multi_ggroc <- function(datalist, fpf_string = rep("FPF", length(datalist)), tpf_string = rep("TPF", length(datalist)), 
                        lty = 1:length(datalist), label = NULL, label.adj.x = rep(0, length(datalist)), 
                        label.adj.y = rep(0, length(datalist)), label.angle = rep(45, length(datalist))){
  
  stopifnot(all(sapply(1:length(datalist), function(i) fpf_string[i] %in% colnames(datalist[[i]]))))
  stopifnot(all(sapply(1:length(datalist), function(i) tpf_string[i] %in% colnames(datalist[[i]]))))
  
  min_br <-  c(seq(0, .1, by = .01), seq(.9, 1, by = .01))
  br <- c(0, .1, .25, .5, .75, .9, 1)
  
  p1 <- ggplot2::ggplot(datalist[[1]], ggplot2::aes_string(x = fpf_string[1], y = tpf_string[1]))  + ggplot2::geom_point(color = "red", alpha = 0) +
    ggplot2::geom_abline(intercept = 0, slope = 1, lty = 1, color = "white") + 
    ggplot2::scale_x_continuous("False positive fraction", minor_breaks = min_br, breaks = br) + 
    ggplot2::scale_y_continuous("True positive fraction", minor_breaks = min_br, breaks = br) + ggplot2::geom_path(lty = lty[1]) 
  
  if(!is.null(label)){
    
    xy <- datalist[[1]][datalist[[1]]$TPF + datalist[[1]]$FPF < 1, c(fpf_string[1], tpf_string[1])][1,]
    X <- xy[1] + label.adj.x[1] + .05
    Y <- xy[2] - .05 + label.adj.y[1]
    p1 <- p1 + ggplot2::geom_text(data = data.frame(FPF = X, TPF = Y, label = label[1]), 
                                  ggplot2::aes_string(x = "FPF", y  = "TPF", label = "label"), angle = label.angle[1])
    
  }
  
  for(i in 2:length(datalist)){
    
    p1 <- p1 + ggplot2::geom_path(data = datalist[[i]], ggplot2::aes_string(x = fpf_string[i], y = tpf_string[i]), lty = lty[i]) + 
      ggplot2::geom_point(data = datalist[[i]], ggplot2::aes_string(x = fpf_string[i], y = tpf_string[i]), color = "red", alpha = 0)
    
    if(!is.null(label)){
      
      xy <- datalist[[i]][datalist[[i]]$TPF + datalist[[i]]$FPF < 1, c(fpf_string[i], tpf_string[i])][1,]
      X <- xy[1] + label.adj.x[i] + .05
      Y <- xy[2] - .05 + label.adj.y[i]
      p1 <- p1 + ggplot2::geom_text(data = data.frame(FPF = X, TPF = Y, label = label[i]), 
                                    ggplot2::aes_string(x = "FPF", y  = "TPF", label = "label"), angle = label.angle[i])
      
    }
    
  }
  
  p1
  
}



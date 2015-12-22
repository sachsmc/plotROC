#' Plot an ROC curve
#' 
#' Deprecated, use \link{geom_roc} instead
#' 
#' @param rocdata Data frame containing true and false positive fractions, and
#'   cutoff values
#' @param fpf_string Column name identifying false positive fraction column
#' @param tpf_string Column name identifying true positive fraction column
#' @param c_string Column name identifying cutoff values
#' @param ci Logical, not supported
#' @param label Not supported
#' @param label.adj.x Not supported
#' @param label.adj.y Not supported
#' @param label.angle Not supported
#' @param plotmath Not supported
#' @param xlabel Defaults to "False positive fraction"
#' @param ylabel Defaults to "True positive fraction"
#' @importFrom grDevices pdf dev.off
#' @importFrom methods slot
#' @importFrom stats qbeta
#' 
#'   
#' @export
#' 
#' @return A ggplot object
#'   

ggroc <- function(rocdata, fpf_string = "FPF", tpf_string = "TPF", c_string = "c", ci = FALSE,
                  label = NULL, label.adj.x = 0, label.adj.y = 0, label.angle = 45, plotmath = FALSE,
                  xlabel = "False positive fraction", ylabel = "True positive fraction"){
  
  message("ggroc is deprecated. Please use geom_roc instead. Type ?geom_roc for more information. ")
  
  if(class(rocdata) == "performance"){
    
    x <- rocdata
    lookup <- c("x.values", "y.values")
    names(lookup) <- c(x@x.name, x@y.name)
    
    tp.fp <- lookup[c("True positive rate", "False positive rate")]
    mydat <- data.frame(TPF = methods::slot(x, tp.fp[1])[[1]], FPF = methods::slot(x, tp.fp[2])[[1]], c = x@alpha.values[[1]])
    rocdata <- subset(mydat, is.finite(c))
    
  }
  
  rocdata <- rocdata[order(rocdata[, c_string]), ]
  stopifnot(fpf_string %in% colnames(rocdata))
  stopifnot(tpf_string %in% colnames(rocdata))
  
  ggplot(rocdata, aes_string(x = fpf_string, y = tpf_string, label = c_string)) + geom_roc(stat = "identity") + 
    style_roc(xlab = xlabel, ylab = ylabel)
  
   
  
}


#' Plot multiple ROC curves
#' 
#' Given a list of results computed by \link{calculate_roc}, plot the curve
#' using ggplot with sensible defaults. Pass the resulting object and data to
#' \link{export_interactive_roc}, \link{plot_interactive_roc}, or
#' \link{plot_journal_roc}.
#' 
#' @param datalist List of data frames each containing true and false positive
#'   fractions and cutoffs
#' @param fpf_string Column names identifying false positive fraction
#' @param tpf_string Column names identifying true positive fraction
#' @param c_string Column names identifying cutoff values
#' @param label Not supported. 
#' @param legend If true, draws legend
#' @param label.adj.x Not supported. 
#' @param label.adj.y Not supported. 
#' @param label.angle Not supported. 
#' @param plotmath Logical. Not supported. 
#' @param xlabel Defaults to "False positive fraction"
#' @param ylabel Defaults to "True positive fraction"
#'   
#' @export
#' 
#' @return A ggplot object
#'   

multi_ggroc <- function(datalist, fpf_string = rep("FPF", length(datalist)), tpf_string = rep("TPF", length(datalist)), 
                        c_string = rep("c", length(datalist)),
                        label = NULL, legend = TRUE, label.adj.x = rep(0, length(datalist)), 
                        label.adj.y = rep(0, length(datalist)), label.angle = rep(45, length(datalist)),
                        plotmath = FALSE, xlabel = "False positive fraction", ylabel = "True positive fraction"){
  
  stopifnot(all(sapply(1:length(datalist), function(i) fpf_string[i] %in% colnames(datalist[[i]]))))
  stopifnot(all(sapply(1:length(datalist), function(i) tpf_string[i] %in% colnames(datalist[[i]]))))
  
  message("ggroc is deprecated. Please use geom_roc instead. Type ?geom_roc for more information. ")
  
  newlist <- lapply(1:length(datalist), function(i){ 
    datalist[[i]]$Biomarker <- LETTERS[i]
    colnames(datalist[[i]])[colnames(datalist[[i]]) == fpf_string] <- "FPF"
    colnames(datalist[[i]])[colnames(datalist[[i]]) == tpf_string] <- "TPF"
    colnames(datalist[[i]])[colnames(datalist[[i]]) == c_string] <- "label"
    datalist[[i]]
    })
  
  longdat <- do.call(rbind, newlist)
  
  ggplot(longdat, aes_string(x = "FPF", y = "TPF", label = "label", color = "Biomarker")) + 
    geom_roc(stat = "identity", show.legend = legend) + style_roc()
  
}



#' Plot an ROC curve for use in print
#' 
#' Deprecated, use \link{style_roc} instead
#' 
#' @param ggroc_p An object as returned by \link{ggroc} or \link{multi_ggroc}.
#'   It can be modified with annotations, themes, etc.
#' @param font.size Not supported
#' @param n.cuts Not supported
#' @param ci.at Not supported
#' @param opacity Not supported
#' @param lty Not supported 
#' @param color Not supported
#' @param lwd Not supported
#' @param legend Not supported
#' 
#'   
#'   
#' @export
#' 
#' @return A ggplot object

plot_journal_roc <- function(ggroc_p, font.size = 3, n.cuts = 20, ci.at = NULL, opacity = .3, 
                             lty = NULL, color = NULL, lwd = NULL, legend = FALSE){
  
  ggroc_p + style_roc()
  
}
  

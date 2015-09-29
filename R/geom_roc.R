#' Calculate the empirical ROC curve
#' 
#' @rdname geom_roc
#' @inheritParams ggplot2::stat_identity
#' 
StatRoc <- ggproto("StatRoc", Stat,
                   required_aes = c("m", "d"), ## biomarker, binary outcome
                   default_aes = aes(x = ..false_positive_fraction.., y = ..true_positive_fraction..),
                   
                   compute_group = function(data, scales){
                     
                     D <- verify_d(data$d)
                     T.order <- order(data$m, decreasing=TRUE)
                     TTT <- data$m[T.order]
                     TPF <- cumsum(D[T.order] == 1)
                     FPF <- cumsum(D[T.order] == 0)
                     
                     ## remove fp & tp for duplicated predictions
                     ## Highest cutoff (Infinity) corresponds to tp=0, fp=0
                     
                     dups <- rev(duplicated(rev(TTT)))
                     tp <- c(0, TPF[!dups])/sum(D == 1)
                     fp <- c(0, FPF[!dups])/sum(D == 0)
                     cutoffs <- c(Inf, TTT[!dups])
                     
                     data.frame(false_positive_fraction = fp, true_positive_fraction = tp, cutoffs = cutoffs)
                     
                   })

#' @export
#' @inheritParams ggplot2::stat_identity


stat_roc <- function(mapping = NULL, data = NULL, geom = "step",
                         position = "identity", show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatRoc,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
  
}


GeomRoc <- ggproto("GeomRoc", GeomStep, 
                   required_aes = c("x", "y"))



geom_roc <- function(mapping = NULL, data = NULL, stat = "roc",
                                position = "identity", show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomRoc, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
  )
}


#' @export
#' @rdname stat_roc

StatRoc <- ggproto("StatRoc", Stat,
                   required_aes = c("m", "d"), ## biomarker, binary outcome
                   default_aes = aes(x = ..false_positive_fraction.., y = ..true_positive_fraction.., label = ..cutoffs..),
                   
                   setup_data = function(data, params){
                     data$d <- verify_d(data$d)
                     data$group <- NULL
                     disc <- vapply(data, is.discrete, logical(1))
                     disc[names(disc) %in% c("label", "PANEL")] <- FALSE
                     
                     if (any(disc)) {
                       data$group <- plyr::id(data[disc], drop = TRUE)
                     } else {
                       data$group <- -1L
                     } 
                   data
                   },
                   compute_group = function(data, scales, na.rm = TRUE){
                     
                     if(na.rm){
                       data <- subset(data, !is.na(d) & !is.na(m))
                     }
                     
                     D <- data$d
                     
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

#' Calculate the empirical Receiver Operating Characteristic curve
#' 
#' Given a binary outcome d and continous measurement m, computes the empirical
#' ROC curve for assessing the classification accuracy of m
#' 
#' @inheritParams ggplot2::stat_identity
#' @param na.rm Remove missing observations
#' @section Aesthetics:
#' \code{stat_roc} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{m}} The continuous biomarker/predictor
#'   \item \strong{\code{d}} The binary outcome, if not coded as 0/1, the 
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#' @section Computed variables:
#' \describe{
#'   \item{false_positive_fraction}{estimate of false positive fraction}
#'   \item{true_positive_fraction}{estimate of true positive fraction}
#'   \item{cutoffs}{values of m at which estimates are calculated}
#' }
#' @export
#' @rdname stat_roc
#' @examples
#' D.ex <- rbinom(50, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 50), rep("B", 50)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + stat_roc()

stat_roc <- function(mapping = NULL, data = NULL, geom = "roc",
                         position = "identity", show.legend = NA, inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    stat = StatRoc,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
  
}

#' @param n.cuts Number of cutpoints to display along each curve
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @param linemitre Line mitre limit (number greater than 1)
#' @param arrow Arrow specification, as created by \code{\link[grid]{arrow}}
#' @param alpha.line Alpha level for the lines
#' @param alpha.point Alpha level for the cutoff points
#' @param size.point Size of cutoff points
#' @param labels Logical, display cutoff text labels
#' @param labelsize Size of cutoff text labels
#' @param labelround Integer, number of significant digits to round cutoff labels
#' @param na.rm Remove missing values from curve
#' @section Computed variables:
#' \describe{
#'   \item{false_positive_fraction}{estimate of false positive fraction}
#'   \item{true_positive_fraction}{estimate of true positive fraction}
#'   \item{cutoffs}{values of m at which estimates are calculated}
#' }
#' @export
#' @rdname geom_roc
#' @examples
#' D.ex <- rbinom(50, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 50), rep("B", 50)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc()
#' \donttest{
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc()
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + facet_wrap(~ Z)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(n.cuts = 20)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(labels = FALSE)
#' }

GeomRoc <- ggproto("GeomRoc", Geom, 
                   required_aes = c("x", "y", "label"), 
                   default_aes = aes(shape = 19, colour = "black", alpha = 1, size = 1, linetype = 1,
                                     angle = 0, hjust = 1,
                                     vjust = 1, family = "", fontface = 1, lineheight = 1.2),
                   non_missing_aes = c("size", "shape"),
                   draw_group = function(data, panel_scales, coord, n.cuts = 10, arrow = NULL,
                                         lineend = "butt", linejoin = "round", linemitre = 1, 
                                         alpha.line = 1, alpha.point = 1,
                                         size.point = .5, labels = TRUE, labelsize = 3.88, labelround = 1,
                                         na.rm = TRUE){
                     
                     if(nrow(data) < n.cuts){ 
                       dex <- 1:nrow(data)
                     } else {
                       dex <- as.integer(seq(1, nrow(data), length.out = n.cuts))
                     }
                     
                     coords <- coord$transform(data, panel_scales)
                     coordsp <- coord$transform(data[dex, ], panel_scales)
                     
                     if(n.cuts > 0) { 
                       pg <- pointsGrob(
                         coordsp$x, coordsp$y,
                         pch = coordsp$shape,
                         size = unit(size.point, "char"),
                         gp = gpar(
                           col = coordsp$colour,
                           fill = coordsp$fill,
                           alpha = alpha.point
                         )
                       )
                      } else pg <- nullGrob()
                     
                     keep <- function(x) {
                       # from first non-missing to last non-missing
                       first <- match(FALSE, x, nomatch = 1) - 1
                       last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
                       c(
                         rep(FALSE, first),
                         rep(TRUE, last - first),
                         rep(FALSE, length(x) - last))
                     }
                     # Drop missing values at the start or end of a line - can't drop in the
                     # middle since you expect those to be shown by a break in the line
                     missing <- !stats::complete.cases(data[c("x", "y", "size", "colour",
                                                              "linetype")])
                     kept <- stats::ave(missing, data$group, FUN = keep)
                     data <- data[kept, ]
                     # must be sorted on group
                     data <- plyr::arrange(data, group)
                     
                     if (!all(kept) && !na.rm) {
                       warning("Removed ", sum(!kept), " rows containing missing values",
                               " (geom_path).", call. = FALSE)
                     }
                     
                     munched <- coord_munch(coord, data, panel_scales)
                     
                     # Silently drop lines with less than two points, preserving order
                     rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                     munched <- munched[rows >= 2, ]
                     if (nrow(munched) < 2) return(zeroGrob())
                     
                     # Work out whether we should use lines or segments
                     attr <- plyr::ddply(munched, "group", function(df) {
                       data.frame(
                         solid = identical(unique(df$linetype), 1),
                         constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
                       )
                     })
                     solid_lines <- all(attr$solid)
                     constant <- all(attr$constant)
                     if (!solid_lines && !constant) {
                       stop("geom_path: If you are using dotted or dashed lines",
                            ", colour, size and linetype must be constant over the line",
                            call. = FALSE)
                     }
                     
                     # Work out grouping variables for grobs
                     n <- nrow(munched)
                     group_diff <- munched$group[-1] != munched$group[-n]
                     start <- c(TRUE, group_diff)
                     end <-   c(group_diff, TRUE)
                     
                     if (!constant) {
                       lg <- segmentsGrob(
                         munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
                         default.units = "native", arrow = arrow,
                         gp = gpar(
                           col = alpha(munched$colour, alpha.line)[!end],
                           fill = alpha(munched$colour, alpha.line)[!end],
                           lwd = munched$size[!end] * .pt,
                           lty = munched$linetype[!end],
                           lineend = lineend,
                           linejoin = linejoin,
                           linemitre = linemitre
                         )
                       )
                     } else {
                       id <- match(munched$group, unique(munched$group))
                       lg <- polylineGrob(
                         munched$x, munched$y, id = id,
                         default.units = "native", arrow = arrow,
                         gp = gpar(
                           col = alpha(munched$colour, alpha.line)[start],
                           fill = alpha(munched$colour, alpha.line)[start],
                           lwd = munched$size[start] * .pt,
                           lty = munched$linetype[start],
                           lineend = lineend,
                           linejoin = linejoin,
                           linemitre = linemitre
                         )
                       )
                     }
                     
                     if(labels & n.cuts > 0){
                       lab <- round(coordsp$label, labelround)
                       
                       if (is.character(coordsp$vjust)) {
                         coordsp$vjust <- compute_just(coordsp$vjust, coordsp$y)
                       }
                       if (is.character(coordsp$hjust)) {
                         coordsp$hjust <- compute_just(coordsp$hjust, coordsp$x)
                       }
                       
                       cg <- textGrob(
                         lab,
                         coordsp$x - .01, coordsp$y + .02, default.units = "native",
                         hjust = coordsp$hjust, vjust = coordsp$vjust,
                         rot = coordsp$angle,
                         gp = gpar(
                           col = alpha(coordsp$colour, coordsp$alpha),
                           fontsize = labelsize * .pt,
                           fontfamily = coordsp$family,
                           fontface = coordsp$fontface,
                           lineheight = coordsp$lineheight
                         )
                       )
                       
                     } else cg <- nullGrob()
                     
                     gList(pg, lg, cg)
                     
                     
                   }, 
                   draw_key = draw_key_path)

#' Empirical Receiver Operating Characteristic Curve
#' 
#' Display the empirical ROC curve. Useful for characterizing the classification
#' accuracy of continuous measurements for predicting binary states
#' 
#' @section Aesthetics:
#' \code{geom_roc} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}} The FPF estimate. This is automatically mapped by \link{stat_roc}
#'   \item \strong{\code{y}} The TPF estimate. This is automatically mapped by \link{stat_roc}
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @param stat Use to override the default connection between
#'   \code{geom_roc} and \code{stat_roc}.
#' @seealso See \code{\link{geom_rocci}} for
#'   displaying rectangular confidence regions for the empirical ROC curve, \code{\link{style_roc}} for 
#'   adding guidelines and labels, and \code{\link{direct_label}} for adding direct labels to the 
#'   curves. Also \link{export_interactive_roc} for creating interactive ROC curve plots for use in a web browser. 
#' @inheritParams ggplot2::geom_point
#' @export
#' 

geom_roc <- function(mapping = NULL, data = NULL, stat = "roc", n.cuts = 10, arrow = NULL,
                     lineend = "butt", linejoin = "round", linemitre = 1, 
                     alpha.line = 1, alpha.point = 1,
                     size.point = .5, labels = TRUE, labelsize = 3.88, labelround = 1,
                     na.rm = TRUE, position = "identity", show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    geom = GeomRoc, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, n.cuts = n.cuts, arrow = arrow,
                     lineend = lineend, linejoin = linejoin, linemitre = linemitre, 
                     alpha.line = alpha.line, alpha.point = alpha.point,
                     size.point = size.point, labels = labels, labelsize = labelsize, labelround = labelround,
                    ...)
  )
}


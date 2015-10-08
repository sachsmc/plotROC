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
                   compute_group = function(data, scales){
                     
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
#' D.ex <- rbinom(250, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(250, mean = D.ex, sd = .4), rnorm(250, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 250), rep("B", 250)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + stat_roc()

stat_roc <- function(mapping = NULL, data = NULL, geom = "roc",
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
#' D.ex <- rbinom(250, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(250, mean = D.ex, sd = .4), rnorm(250, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 250), rep("B", 250)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc()
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc()
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + facet_wrap(~ Z)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(n.cuts = 50)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(labels = FALSE)

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
                                         na.rm = FALSE){
                     
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
#' @param geom,stat Use to override the default connection between
#'   \code{geom_roc} and \code{stat_roc}.
#' @seealso See \code{\link{geom_rocci}} for
#'   displaying rectangular confidence regions for the empirical ROC curve, \code{\link{style_roc}} for 
#'   adding guidelines and labels, and \code{\link{direct_label}} for adding direct labels to the 
#'   curves. Also \link{export_interactive_roc} for creating interactive ROC curve plots for use in a web browser. 
#' @inheritParams ggplot2::geom_point
#' @export
#' 

geom_roc <- function(mapping = NULL, data = NULL, stat = "roc",
                                position = "identity", show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomRoc, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
  )
}



#' Calculate confidence regions for the empirical ROC curve
#' 
#' Confidence intervals for TPF and FPF are calculated using the exact 
#'   method of Clopper and Pearson (1934) each at the level \code{1 - sqrt(1 - 
#'   alpha)}. Based on result 2.4 from Pepe (2003), the cross-product of these 
#'   intervals yields a 1 - alpha % confidence region for (FPF, TPF).
#' 
#' @rdname stat_rocci
#' @section Aesthetics: 
#' \code{stat_rocci} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{m}} The continuous biomarker/predictor
#'   \item \strong{\code{d}} The binary outcome, if not coded as 0/1, the 
#'   smallest level in sort order is assumed to be 0, with a warning
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{FPF}{estimate of false positive fraction}
#'   \item{TPF}{estimate of true positive fraction}
#'   \item{cutoffs}{values of m at which estimates are calculated}
#'   \item{FPFL}{lower bound of confidence region for FPF}
#'   \item{FPFU}{upper bound of confidence region for FPF}
#'   \item{TPFL}{lower bound of confidence region for TPF}
#'   \item{TPFU}{upper bound of confidence region for TPF}
#' }
#' @export
#' @rdname stat_roc
#' @references \itemize{
#' \item Clopper, C. J., and Egon S. Pearson. "The use of confidence or fiducial limits illustrated in the case of the binomial." Biometrika (1934): 404-413.
#' \item Pepe, M.S. "The Statistical Evaluation of Medical Tests for Classification and Prediction." Oxford (2003). 
#' }
#' @inheritParams ggplot2::stat_identity
#' @export
#' @examples
#' D.ex <- rbinom(250, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(250, mean = D.ex, sd = .4), rnorm(250, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 250), rep("B", 250)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + stat_rocci()
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + 
#' stat_rocci(ci.at = quantile(rocdata$M, c(.1, .3, .5, .7, .9)))
#' 
StatRocci <- ggproto("StatRocci", Stat,
                   required_aes = c("m", "d"), ## biomarker, binary outcome
                   default_aes = aes(x = ..FPF.., y = ..TPF.., 
                                     xmin = ..FPFL.., xmax = ..FPFU.., ymin = ..TPFL.., ymax = ..TPFU.., label = ..cutoffs..),
                   
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
                   
                   compute_group = function(data, scales, ci.at = NULL, sig.level = .05){
                     
                     D <- verify_d(data$d)
                     T.order <- order(data$m, decreasing=TRUE)
                     TTT <- data$m[T.order]
                     
                     stopifnot(is.finite(sig.level) && sig.level < 1 && sig.level > 0)
                     
                     if(is.null(ci.at)){ # choose some evenly spaced locations
                       
                       ci.dex <- as.integer(seq(1, nrow(data), length.out = 5))[-c(1, 5)]
                       ci.loc <- unique(TTT)[ci.dex]
                       
                     } else ci.loc <- ci.at
                     
                     alpha.star <- 1 - sqrt(1 - sig.level)
                     n0 <- sum(D == 0)
                     n1 <- sum(D == 1)
                     M0 <- data$m[D == 0]
                     M1 <- data$m[D == 1]
                     
                     ci_res <- sapply(ci.loc, function(x){ 
                       
                       
                       FP.L <- qbeta(alpha.star, sum(M0 > x), n0 - sum(M0 > x) + 1)
                       FP.U <- qbeta(1  - alpha.star, sum(M0 > x) + 1, n0 - sum(M0 > x))
                       
                       TP.L <- qbeta(alpha.star, sum(M1 > x), n1 - sum(M1 > x) + 1)
                       TP.U <- qbeta(1  - alpha.star, sum(M1 > x) + 1, n1 - sum(M1 > x))
                       
                       FPF <- mean(M0 > x)
                       TPF <- mean(M1 > x)
                       
                       c(FP.L, FP.U, TP.L, TP.U, FPF, TPF, x)
                       
                     })
                     
                     ci_res2 <- as.data.frame(t(ci_res))
                     colnames(ci_res2) <- c("FPFL", "FPFU", "TPFL", "TPFU", "FPF", "TPF", "cutoffs")
                     ci_res2
                     
                     
                   })

#' @export
#' @rdname stat_rocci
#' @inheritParams ggplot2::stat_identity
#' @param ci.at Vector of cutoffs at which to display confidence regions. If
#'   NULL, will automatically choose 3 evenly spaced points to display the regions
#'   @param sig.level Significance level for the confidence regions


stat_rocci <- function(mapping = NULL, data = NULL, geom = "rocci",
                     position = "identity", show.legend = NA, inherit.aes = TRUE, ci.at = NULL, sig.level = .05, ...) {
  layer(
    stat = StatRocci,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(ci.at = ci.at, sig.level = sig.level, ...)
  )
  
}

#' Confidence regions for the ROC curve
#' 
#' Display rectangular confidence regions for the empirical ROC curve. 
#' 
#' @section Aesthetics:
#' \code{geom_rocci} understands the following aesthetics (required aesthetics
#' are in bold). \code{stat_rocci} automatically maps the estimates to the required aesthetics:
#' \itemize{
#'   \item \strong{\code{x}} The FPF estimate
#'   \item \strong{\code{y}} The TPF estimate
#'   \item \strong{\code{xmin}} Lower confidence limit for the FPF
#'   \item \strong{\code{xmax}} Upper confidence limit for the FPF
#'   \item \strong{\code{ymin}} Lower confidence limit for the TPF
#'   \item \strong{\code{ymax}} Upper confidence limit for the TPF
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @param geom,stat Use to override the default connection between
#'   \code{geom_rocci} and \code{stat_rocci}.
#' @seealso See \code{\link{geom_roc}} for the empirical ROC curve, \code{\link{style_roc}} for 
#'   adding guidelines and labels, and \code{\link{direct_label}} for adding direct labels to the 
#'   curves. Also \link{export_interactive_roc} for creating interactive ROC curve plots for use in a web browser. 
#' @inheritParams ggplot2::geom_point
#' @export

geom_rocci <- function(mapping = NULL, data = NULL, stat = "rocci",
                       position = "identity", show.legend = NA, 
                       inherit.aes = TRUE, ci.at = NULL, sig.level = .05, ...) {
  layer(
    geom = GeomRocci, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ci.at = ci.at, sig.level = sig.level, ...)
  )
}

#' @param alpha.box Alpha level for the confidence regions
#' @param labels If TRUE, adds text labels for the cutoffs where the confidence regions are displayed
#' @param labelsize Size of cutoff text labels
#' @param labelround Integer, number of significant digits to round cutoff labels
#' @export
#' @rdname geom_rocci
#' @examples 
#' 
#' D.ex <- rbinom(250, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(250, mean = D.ex, sd = .4), rnorm(250, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 250), rep("B", 250)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + geom_rocci()
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci()
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci(sig.level = .01)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(n.cuts = 0) + 
#' geom_rocci(ci.at = quantile(rocdata$M, c(.1, .25, .5, .75, .9)))
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci(linetype = 1)


GeomRocci <- ggproto("GeomRocci", Geom, 
                   required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax", "label"), 
                   default_aes = aes(size = .5, shape = 19, fill = "black",
                                     angle = 0, hjust = 1, linetype = 0,
                                     vjust = 1, family = "", fontface = 1, lineheight = 1.2),
                   non_missing_aes = c("size", "shape", "fill"),
                   draw_group = function(data, panel_scales, coord, alpha.box = .3, 
                                         labels = TRUE, labelsize = 3.88, labelround = 1){
                     
                       coords <- coord$transform(data, panel_scales)
                        
                       rg <- rectGrob(
                         coords$xmin, coords$ymax,
                         width = coords$xmax - coords$xmin,
                         height = coords$ymax - coords$ymin,
                         #default.units = "native",
                         just = c("left", "top"),
                         gp = gpar(
                           col = coords$colour,
                           fill = alpha(coords$fill, alpha.box),
                           lwd = coords$size * .pt,
                           lty = coords$linetype,
                           lineend = "butt"
                         )
                       )
                     
                     if(length(coords$x) > 0) { 
                       pg <- pointsGrob(
                         coords$x, coords$y,
                         pch = coords$shape,
                         size = unit(coords$size, "char"),
                         gp = gpar(
                           col = coords$colour,
                           fill = coords$fill
                         )
                       )
                     } else pg <- nullGrob()
                     
              
                       if(labels & length(coords$x) > 0){
                         lab <- round(coords$label, labelround)
                         
                         if (is.character(coords$vjust)) {
                           coords$vjust <- compute_just(coords$vjust, coords$y)
                         }
                         if (is.character(coords$hjust)) {
                           coords$hjust <- compute_just(coords$hjust, coords$x)
                         }
                         
                         cg <- textGrob(
                           lab,
                           coords$x - .01, coords$y + .02, default.units = "native",
                           hjust = coords$hjust, vjust = coords$vjust,
                           rot = coords$angle,
                           gp = gpar(
                             col = coords$colour,
                             fontsize = labelsize * .pt,
                             fontfamily = coords$family,
                             fontface = coords$fontface,
                             lineheight = coords$lineheight
                           )
                         )
                         
                       } else cg <- nullGrob()
                     
                     gList(pg, rg, cg)
                     
                     
                   }, 
                   draw_key = draw_key_polygon)





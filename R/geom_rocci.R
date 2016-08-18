

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
#' D.ex <- rbinom(50, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 50), rep("B", 50)))
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
                     
                     compute_group = function(data, scales, ci.at = NULL, sig.level = .05, na.rm = TRUE){
                       
                       if(na.rm){
                         data <- subset(data, !is.na(d) & !is.na(m))
                       }
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
#' @param sig.level Significance level for the confidence regions
#' @param na.rm Remove missing observations


stat_rocci <- function(mapping = NULL, data = NULL, geom = "rocci",
                       position = "identity", show.legend = NA, inherit.aes = TRUE, ci.at = NULL, sig.level = .05, na.rm = TRUE, ...) {
  layer(
    stat = StatRocci,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(ci.at = ci.at, sig.level = sig.level, na.rm = na.rm, ...)
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
#' @inheritParams ggplot2::geom_point
#' @param stat Use to override the default connection between
#'   \code{geom_rocci} and \code{stat_rocci}.
#' @param ci.at Vector of values in the range of the biomarker where confidence regions will be displayed
#' @param sig.level Significance level for the confidence regions
#' @export

geom_rocci <- function(mapping = NULL, data = NULL, stat = "rocci", ci.at = NULL, sig.level = .05, na.rm = TRUE, 
                       alpha.box = .3, labels = TRUE, labelsize = 3.88, labelround = 1,
                       position = "identity", show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    geom = GeomRocci, mapping = mapping, data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(ci.at = ci.at, sig.level = sig.level, na.rm = na.rm, 
                  alpha.box = alpha.box, labels = labels, labelsize = labelsize, 
                  labelround = labelround, ...)
  )
}

#' @param alpha.box Alpha level for the confidence regions
#' @param labels If TRUE, adds text labels for the cutoffs where the confidence regions are displayed
#' @param labelsize Size of cutoff text labels
#' @param labelround Integer, number of significant digits to round cutoff labels
#' @seealso See \code{\link{geom_roc}} for the empirical ROC curve, \code{\link{style_roc}} for 
#'   adding guidelines and labels, and \code{\link{direct_label}} for adding direct labels to the 
#'   curves. Also \link{export_interactive_roc} for creating interactive ROC curve plots for use in a web browser. 
#' @export
#' @rdname geom_rocci
#' @examples 
#' 
#' D.ex <- rbinom(50, 1, .5)
#' rocdata <- data.frame(D = c(D.ex, D.ex), 
#'                    M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)), 
#'                    Z = c(rep("A", 50), rep("B", 50)))
#'
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc() + geom_rocci()
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci()
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci(sig.level = .01)
#' ggplot(rocdata, aes(m = M, d = D)) + geom_roc(n.cuts = 0) + 
#' geom_rocci(ci.at = quantile(rocdata$M, c(.1, .25, .5, .75, .9)))
#' ggplot(rocdata, aes(m = M, d = D, color = Z)) + geom_roc() + geom_rocci(linetype = 1)


GeomRocci <- ggproto("GeomRocci", Geom, 
                     required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax", "label"), 
                     default_aes = aes(size = 1, shape = 4, fill = "black",
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
                     draw_key = draw_key_blank)





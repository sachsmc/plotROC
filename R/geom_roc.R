#' Calculate the empirical ROC curve
#' 
#' @rdname geom_roc
#' @inheritParams ggplot2::stat_identity
#' @export
#' 
StatRoc <- ggproto("StatRoc", Stat,
                   required_aes = c("m", "d"), ## biomarker, binary outcome
                   default_aes = aes(x = ..false_positive_fraction.., y = ..true_positive_fraction.., label = ..cutoffs..),
                   
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

#' @export

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
#' @rdname geom_rocci
#' @inheritParams ggplot2::stat_identity
#' @export
#' 
StatRocci <- ggproto("StatRocci", Stat,
                   required_aes = c("m", "d"), ## biomarker, binary outcome
                   default_aes = aes(x = ..FPF.., y = ..TPF.., 
                                     xmin = ..FPFL.., xmax = ..FPFU.., ymin = ..TPFL.., ymax = ..TPFU.., label = ..cutoffs..),
                   
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
#' @inheritParams ggplot2::stat_identity


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





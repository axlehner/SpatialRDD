


#' Plot SpatialRD output
#'
#'Produces plot of GRDDseries and optionally of a map that visualises every point estimate in space
#' @param SpatialRDoutput spatial obkect that is produce by an estimation with \code{\link{SpatialRD}}
#' @param map T/F depending on whether mapplot is desired
#'
#' @return plots produced with ggplot 2
#' @export
#'
#' @examples
plotspatialrd <- function(SpatialRDoutput = results, map = T) {

  # replaced Courier New with Courier for now
  # TODO

  # - make pvalue an ggplot2::aes() with more intensity of colour depending on p-value
  # - numerate borderpoints in mapplot
  # - is GRDDseries the right title?
  # - bring y name in the plot. instead of "Point-Estimate" on y axis?

  GRDD <- ggplot2::ggplot(data = SpatialRDoutput,
                 mapping = ggplot2::aes(x = Point, y = Estimate, ymin = CI_Conv_l, ymax = CI_Conv_u)) +
    ggplot2::geom_errorbar(color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_point(ggplot2::aes(colour = cut(pvalC, c(-Inf, .11, Inf))), size = 1, shape = 19) +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
    ggplot2::ggtitle(paste("GRDDseries (conventional)")) +
    ggplot2::labs(y = "Point-Estimate", x = "#Boundarypoint [conv. confidence intervals]")


  GRDDrob <- ggplot2::ggplot(data = SpatialRDoutput,
                    mapping = ggplot2::aes(x = Point, y = Estimate, ymin = CI_Rob_l, ymax = CI_Rob_u)) +
    ggplot2::geom_errorbar(color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_point(ggplot2::aes(colour = cut(pvalR, c(-Inf, .11, Inf))), size = 1, shape = 19) +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
    ggplot2::ggtitle(paste("GRDDseries (robust)")) +
    ggplot2::labs(y = "Point-Estimate", x = "#Boundarypoint [rob. confidence intervals]")


  # MAPPLOT OF BORDERPOINTS
  mapplot <- ggplot2::ggplot() +
    #geom_sf(data = polygon_full.sf, alpha = 0.5) + # u need the data = !
    ggplot2::geom_sf(data = SpatialRDoutput, ggplot2::aes(colour = cut(pvalC, c(-Inf, .11, Inf))), size = 1, shape = 19) + #coord_equal() +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    #geom_point(data = data, ggplot2::aes(longitude, latitude), size = 0.5) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", axis.title.y = ggplot2::element_blank()) +
    ggplot2::ggtitle("conv. inference")
  #coord_map(xlim = c(73.7, 74.2), ylim = c(15, 15.8))


  if (map == T) {
    cowplot::plot_grid(cowplot::plot_grid(GRDD, GRDDrob, align = "v", nrow = 2), mapplot, rel_widths = c(1.8, 1))
  } else {
    cowplot::plot_grid(GRDD, GRDDrob, align = "v", nrow = 2)
  }

}

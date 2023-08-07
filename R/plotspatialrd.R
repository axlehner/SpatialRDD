


#' Plot SpatialRD output
#'
#'Produces plot of GRDDseries and optionally of a map that visualises every point estimate in space.
#'
#' @param SpatialRDoutput spatial object that is produced by an estimation with \code{\link{spatialrd}}
#' @param map TRUE/FALSE depending on whether mapplot is desired (make sure to set \code{spatial.objcet = TRUE} in the \code{\link{spatialrd}} function)
#'
#' @return plots produced with ggplot2
#' @export
#'
#' @examples
#'
#' points_samp.sf <- sf::st_sample(polygon_full, 1000) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' # add a unique ID to each observation:
#' points_samp.sf$id <- 1:nrow(points_samp.sf)
#' # assign treatment:
#' points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
#' # first we define a variable for the number of "treated" and control
#' NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
#' NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
#' # the treated areas get a 10 percentage point higher literacy rate
#' points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
#' points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
#' # and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
#' points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
#'   points_samp.sf$education[points_samp.sf$treated == 1]
#' points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
#'   points_samp.sf$education[points_samp.sf$treated == 0]
#'
#' # create distance to cutoff
#' points_samp.sf$dist2cutoff <- as.numeric(sf::st_distance(points_samp.sf, cut_off))
#'
#' points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
#' # give the non-treated one's a negative score
#' points_samp.sf$distrunning[points_samp.sf$treated == 0] <- -1 *
#'  points_samp.sf$distrunning[points_samp.sf$treated == 0]
#'
#' # create borderpoints
#' borderpoints.sf <- discretise_border(cutoff = cut_off, n = 10)
#' borderpoints.sf$id <- 1:nrow(borderpoints.sf)
#'
#' # finally, carry out estimation alongside the boundary:
#' results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf,
#' treated = "treated", minobs = 20, spatial.object = FALSE)
#'
#'
#' plotspatialrd(results)

plotspatialrd <- function(SpatialRDoutput, map = FALSE) {

  # replaced Courier New with Courier for now
  # TODO

  # - make pvalue an ggplot2::aes() with more intensity of colour depending on p-value
  # - numerate borderpoints in mapplot
  # - is GRDDseries the right title?
  # - bring y name in the plot. instead of "Point-Estimate" on y axis?

  GRDD <- ggplot2::ggplot(data = SpatialRDoutput,
                 mapping = ggplot2::aes(x = .data$Point, y = .data$Estimate, ymin = .data$CI_Conv_l, ymax = .data$CI_Conv_u)) +
    ggplot2::geom_errorbar(color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_point(ggplot2::aes(colour = cut(.data$p_Conv, c(-Inf, .11, Inf))), size = 1, shape = 19) +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
    ggplot2::ggtitle(paste("GRDDseries (conventional)")) +
    ggplot2::labs(y = "Point-Estimate", x = "#Boundarypoint [conv. confidence intervals]")


  GRDDrob <- ggplot2::ggplot(data = SpatialRDoutput,
                    mapping = ggplot2::aes(x = .data$Point, y = .data$Estimate, ymin = .data$CI_Rob_l, ymax = .data$CI_Rob_u)) +
    ggplot2::geom_errorbar(color = "grey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ggplot2::geom_point(ggplot2::aes(colour = cut(.data$p_Rob, c(-Inf, .11, Inf))), size = 1, shape = 19) +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
    ggplot2::ggtitle(paste("GRDDseries (robust)")) +
    ggplot2::labs(y = "Point-Estimate", x = "#Boundarypoint [rob. confidence intervals]")


  # MAPPLOT OF BORDERPOINTS
  mapplot <- ggplot2::ggplot() +
    #geom_sf(data = polygon_full.sf, alpha = 0.5) + # u need the data = !
    ggplot2::geom_sf(data = SpatialRDoutput, ggplot2::aes(colour = cut(.data$p_Conv, c(-Inf, .11, Inf))), size = 1, shape = 19) + #coord_equal() +
    ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
    #geom_point(data = data, ggplot2::aes(longitude, latitude), size = 0.5) +
    # Here comes the styling
    ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
    ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none", axis.title.y = ggplot2::element_blank()) +
    ggplot2::ggtitle("conv. inference")
  #coord_map(xlim = c(73.7, 74.2), ylim = c(15, 15.8))


  if (map == TRUE) {
    cowplot::plot_grid(cowplot::plot_grid(GRDD, GRDDrob, align = "v", nrow = 2), mapplot, rel_widths = c(1.8, 1))
  } else {
    cowplot::plot_grid(GRDD, GRDDrob, align = "v", nrow = 2)
  }

}

# corrected T/F

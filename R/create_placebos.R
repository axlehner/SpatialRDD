


#' Multiple placebocheks unified in just one list or coefplot
#'
#' Unifies \code{\link{shift_border}}, \code{\link{cutoff2polygon}}, \code{\link{assign_treated}} in one function to carry out a myriad of placebo checks at once.
#' The output is either a data.frame (with or without geometry of the respective placeboline) or a coefplot.
#' Requires operations data.frame that contains all desired operations (columns shift.x, shift.y, scale, angle, orientation.1, orientation.2, corners, endpoint.1, endpoint.2),
#' if you don't need a certain operation just use default values (e.g. 0 for angle and 1 for scale), but the column has to be there.
#'
#' @param data sf data.frame that contains all units of observation
#' @param cutoff initial RD cutoff as an sj object
#' @param formula provide the formula you want to use for OLS, omit the treatetment dummy (if you want a univariate regression just on "treated", then provide y ~ 1 as formula)
#' @param operations container that has all the information in it on how to change the border for each placeboregression
#' @param coefplot provide coefplot instead of a data.frame
#' @param geometry set to \code{TRUE} if you want to plot all the lines of the used placebo borders
#'
#' @return either a coefplot or data.frame containing results of placeboregressions
#' @export
#'
#' @examples
#' \dontrun{create_placebos(data = points_samp.sf, cutoff = cut_off.sf,
#' formula = education ~ 1, operations = operations)}

create_placebos <- function(data, cutoff, formula, operations,
                             coefplot = F, geometry = F
                             ) {

  # TODO
  # - so far it cannot accomodate the initial estimate with the initial treatment polygon
  # - on which side is the treated - have to define that in the absence of a treatment polygon

  # CRS CHECKS FIRST
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "cutoff not an sf object" = inherits(cutoff, "sf"),
    "CRS not matching between objects, transform them accordingly!"
    = sf::st_crs(data)$input == sf::st_crs(cutoff)$input,

    "formula not provided correctly" = rlang::is_formula(formula)
  )

  bw_dist <- 3000 # make this a parameter later
  # create the container -------------------------
  nruns <- nrow(operations)
  columnames <- c("id", "estimate", "std.error", "statistic", "p.value", "geometry") # should we extract Ntr + Nco?
  results <- data.frame(matrix(ncol = length(columnames), nrow = nruns))
  colnames(results) <- columnames
  results$id <- 1:nruns

  # create formula from input ------------
  # could equally well say: provide formula and then automatically update in the treated.1 in first posi
  formula <- stats::update(formula, ~ treated + .) # treated is always put on second position

  # loop over the placeboregressions -------------------------
  # ... later: option to parallelise! foreach or just make this loop one function and run it on parLapply or the purrr equiv? (since we have many params)
  for (i in 1:nrow(operations)) {
    print(operations[i,])
    cutoff.1      <- shift_border(cutoff, operation = c("shift", "angle", "scale"),
                                  shift = c(operations$shift.x[i], operations$shift.y[i]),
                                  scale = operations$scale[i],
                                  angle = operations$angle[i],
                                  messages = F)
    polygon.1     <- cutoff2polygon(data = data, cutoff = cutoff.1,
                                    orientation = c(operations$orientation.1[i], operations$orientation.2[i]),
                                    corners     = operations$corners[i],
                                    endpoints   = c(operations$endpoint.1[i], operations$endpoint.2[i]),
                                    messages = F)
    data$treated <- assign_treated(data = data, polygon = polygon.1, id = "id")

    data$dist2cutoff.1 <- as.numeric(sf::st_distance(data, cutoff.1)) # compute distance to new border
    results[i, 2:5] <- broom::tidy(stats::lm(formula, data = data[data$dist2cutoff.1 < bw_dist, ]))[2, 2:5] # second regressor extracted (the treated)
    if (geometry == T) results$geometry[i] <- sf::st_geometry(cutoff.1)
  }
  # make it an sf object if we want to plot the lines on a map
  if (geometry == T) results <- sf::st_sf(results, crs = sf::st_crs(data)$input) # this finds the geom column automatically


  if (coefplot == T) {
    results %>% ggplot2::ggplot(mapping = ggplot2::aes(x = .data$id, y = .data$estimate,
                                                       ymin = (.data$estimate - 1.96 * .data$std.error),
                                                       ymax = (.data$estimate + 1.96 * .data$std.error))) +
      ggplot2::geom_errorbar(color = "grey") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      ggplot2::geom_point(ggplot2::aes(colour = cut(.data$p.value, c(-Inf, .11, Inf))), size = 1, shape = 19) +
      ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
      # Here comes the styling
      ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
      ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
      ggplot2::ggtitle(paste("Coefplot")) +
      ggplot2::labs(y = expression(beta~(treated)), x = "boundaryshift")

  } else results # if no coefplot requested we just return the data.frame with results

}

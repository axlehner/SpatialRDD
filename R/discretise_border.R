#' Discretise the RDD cut-off
#'
#' Takes in a border in the form of a polyline (or borderpoints) and converts it into point data.
#' These points are later used to calculate a heterogenous treatment effect alongside the border (i.e. the cut-off)
#'
#'
#'
#' @param cutoff sf object of the RD cut-off in the form of a line (not preferred, but also boundarypoints are possible)
#' @param n the number of borderpoints to be created
#' @param random whether they are randomly chosen (not desireable in most cases)
#' @param range default = F, if there is a specific range (N-S or E-W) for which the points are to be drawn (useful in order to exclude sparse borderpoints with little/no oberservations around because the non-parametric RD estimation will fail)
#' @param ymax if range = T: y coordinates
#' @param ymin if range = T: y coordinates
#' @param xmax if range = T: x coordinates
#' @param xmin if range = T: x coordinates
#'
#' @return an sf object with selected and evenly spaced borderpoints
#' @export
#'
#' @examples
#' borderpoints.sf <- discretise_border(cutoff = cut_off.sf, n = 50)
discretise_border <- function(cutoff = cut_off.sf, n = 10, random = F, range = F, ymax = NA, ymin = NA, xmax = NA, xmin = NA) {

  # do I need to prepare for other cases where the input is neither linestring nor multilinestring?
  # here the case for line data
  if (sf::st_geometry_type(cutoff)[1] == "LINESTRING" | sf::st_geometry_type(cutoff)[1] == "MULTILINESTRING") {
    cat("Starting to create", n, "borderpoints. Approximately every", round(as.numeric(sf::st_length(cutoff))/1000/n, 0), "kilometres we can run an estimation then.\n")
    if (random == F) {
      cutoff <- cutoff %>% sf::st_cast("LINESTRING")
      borderpoints.sf <- sf::st_line_sample(cutoff, n)
    } else {
      borderpoints.sf <- sf::st_sample(cutoff, n)
    }

  } else { # this is the case for points (subsetting)

    cutoff$id <- 1:length(cutoff)
    borderpoints.sf <- cutoff[seq(1, nrow(cutoff), round(nrow(cutoff) / n, 0)), ] # subset according to this rule
    cat("Starting to create", n, "borderpoints. Approximately every", round(as.numeric(sf::st_distance(borderpoints.sf[1], borderpoints.sf[2]))/1000/n, 0), "kilometres we can run an estimation then.\n")


  }

  if (range == T) {



  }
  # need single points so we can loop over them afterwards
  borderpoints.sf <- borderpoints.sf %>% sf::st_cast("POINT")
  borderpoints.sf

}


#' Split the RD cut-off into borderpoints
#'
#' Takes in a border in the form of a polyline (or borderpoints) and converts it into point data.
#' These points are later used to run separate non-parametric RD estimations which eventually allows to visualise potential heterogeneous treatment effects alongside the cut-off.
#'
#'
#'
#' @param cutoff sf object of the RD cut-off in the form of a line (not preferred, but also boundarypoints are possible)
#' @param n the number of borderpoints to be created
#' @param random whether they are randomly chosen (not desireable in most cases)
#' @param range default = FALSE, if there is a specific range (N-S or E-W) for which the points are to be drawn (useful in order to exclude sparse borderpoints with little/no oberservations around because the non-parametric RD estimation will fail)
#' @param ymax if range = TRUE: y coordinates
#' @param ymin if range = TRUE: y coordinates
#' @param xmax if range = TRUE: x coordinates
#' @param xmin if range = TRUE: x coordinates
#'
#' @return an sf object with selected (and evenly spaced) borderpoints
#' @export
#'
#' @examples
#' borderpoints <- discretise_border(cutoff = cut_off, n = 10)
#'
discretise_border <- function(cutoff, n = 10, random = FALSE, range = FALSE, ymax = NA, ymin = NA, xmax = NA, xmin = NA) {

  # TODO
  # - fix the discrepancies btween just sfc and a full sf with multiple columns (length doesn't work then)
  # - implement the range where user can decide the bounds in each direction from which to sample
  # - line_sample TAKES ONE POINT PER FEATURE, NEED TO MAKE IT A SINGLE LINE!
  # THIS RESULTED IN A CRACY MESS
  # - resolve the length vs nrow issue in the bottom for POINTS

  stopifnot(
    "cutoff is not an sf object"            = inherits(cutoff, "sf")
  )

  # do I need to prepare for other cases where the input is neither linestring nor multilinestring?
  # here the case for line data
  if (sf::st_geometry_type(cutoff)[1] == "LINESTRING" | sf::st_geometry_type(cutoff)[1] == "MULTILINESTRING") {
    #message("Starting to create", n, "borderpoints from the given set of borderpoints. Approximately every", round(as.numeric(sf::st_length(cutoff))/1000/n, 0), "kilometres we can run an estimation then.\n")

    # first let's make the line a one feature object so that the sampling works (it draws n elements per feature with st_line_sample)
    # using combine in this version, might be risky because of errors
    #cutoff <- sf::st_combine(cutoff)



    if (random == FALSE) {
      cutoff <- cutoff %>% sf::st_cast("LINESTRING")
      #cutoff <- sf::st_combine(cutoff) # again?
      # this is the dirty hack when someone puts in 4326
      if (sf::st_crs(cutoff)$input == "EPSG:4326" | is.na(sf::st_crs(cutoff)[1])) {
        # second condition is needed because some CRS don't have an EPSG code
        cutoff <- sf::st_transform(cutoff, 3857) # transform it
        cutoff <- sf::st_combine(cutoff) # again?
        cutoff <- cutoff %>% sf::st_cast("LINESTRING") # cast again
        borderpoints.sf <- sf::st_line_sample(cutoff, n)
        borderpoints.sf <- sf::st_transform(borderpoints.sf, 4326)
      } else { # otherwise go for the only way we had in v 0.9000
        borderpoints.sf <- sf::st_line_sample(cutoff, n)
      }

    } else {
      borderpoints.sf <- sf::st_sample(cutoff, n)
    }

  } else { # this is the case for points (subsetting)

    # hack to overcome problem when vec or matrix
    #if ()
    cutoff$id <- 1:length(cutoff) # was this the hack to make the nrow work?
    borderpoints.sf <- cutoff[seq(1, nrow(cutoff), round(nrow(cutoff) / n, 0)), ] # subset according to this rule
    #message("Starting to create", n, "borderpoints from the given line. Approximately every", round(as.numeric(sf::st_distance(borderpoints.sf[1], borderpoints.sf[2]))/1000/n, 0), "kilometres we can run an estimation then.\n")


  }

  if (range == TRUE) {



  }
  # need single points so we can loop over them afterwards
  borderpoints.sf <- borderpoints.sf %>% sf::st_cast("POINT")
  #sf::st_crs(borderpoints.sf) <- sf::st_crs(cutoff) # just to be sure that we are getting the right crs, hope this doesn't create conflict with vignette example
  sf::st_sf(borderpoints.sf)

}

# fix TRUE FALSE
# make example active
# supressed messages

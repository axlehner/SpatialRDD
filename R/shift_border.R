#' Shift, shrink/grow, and rotate borders around
#'
#' This functions takes in a border and can either shift, shrink, or rotate it. All of them can be done together as well.
#' This usually takes a bit of trial and error, so make sure to plot the result each time.
#' For a detailed walk through check out the according vignette: \code{vignette(shifting_borders)}.
#'
#' @param border sf object with line geometry
#' @param operation \code{"shift"}, \code{"rotate"}, \code{"scale"} - or a combination of them
#' @param shift if \code{operation = "shift"}, shift distance in CRS units (if UTM it is metres) for x and y coordinates as \code{c(dist_x, dist_y)}
#' @param angle if \code{operation = "rotate"}, provide angle in degrees
#' @param scale if \code{operation = "scale"}, provide shrinkage/growth factor: e.g. \code{.9} to shrink by 10perc. and \code{1.1} to increase by 10perc.
#'
#' @return a new border in the form of an sf object
#' @export
#'
#' @examples
#' shift_border(border = cut_off, operation = c("shift", "scale"),
#' shift = c(-5000, -3000), scale = .85)
#'
#' shift_border(border = cut_off, operation = "rotate", angle = 10)
#'
#'

shift_border <- function(border, operation = c("shift", "scale", "rotate"),
                           shift = c(0, 0), scale = 1, angle = 0) {

  # TODO
  # - naming not consistent here with other functions (here used border instead of cutoff) -> minor issue, but good to have it general for other designs (where there is no cutoff)

  stopifnot(
    "cutoff is not an sf object"            = inherits(border, "sf")
  )
  #if (messages == TRUE) message("Pay attention to CRS! If you work in lon/lat then degrees have to be provided. Local UTM CRS is preferable!\n")

  # first we take out the geometry and the centroid
  border_sfc <- sf::st_geometry(border)
  border_centroid_sfc <- sf::st_centroid(border_sfc)

  #----------------------------------------------------------------------
  if ("shift" %in% operation) {

    #if (st_geometry_type(border)[1] == "POINT") { # cases for the different types, but we prbly don't need

    border_sfc <- border_sfc + c(shift[1], shift[2]) # units are in deg or meters here

    #} else {
    #  message("nothing of use provided")
    #}


  }
  #----------------------------------------------------------------------
  if ("scale" %in% operation) {

    border_sfc <- (border_sfc - border_centroid_sfc) * scale + border_centroid_sfc

  }
  #----------------------------------------------------------------------
  if ("rotate" %in% operation) {

    # this is a rotation matrix that takes degrees
    rotation <- function(a) {
      r <- a * pi / 180 # degrees to radians
      matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
    }

    border_sfc <- (border_sfc - border_centroid_sfc) * rotation(angle) + border_centroid_sfc

  }

  # then we convert the sfc into an sf? JUNE19: seems we don't need, causes bug with KT15 border, therefore removed
  #border_new <- sf::st_set_geometry(border, border_sfc)
  #sf::st_crs(border_new) <- sf::st_crs(border)
  sf::st_crs(border_sfc) <- sf::st_crs(border)
  sf::st_sf(border_sfc) # changed this to sf in June20
}

# corrected T/F
# activated the example run (remove \dontrun{})



# input should be able to be point or line
# lonlat plus utm local
# need to assign treated, then report how many treated ones flipped the status
## how? polygon (rescale equally), or with the above/under rule from stackexchange?

## also for the SpatialRD this matters quite a lot

# putput should always be a polygon so that we can assign the treated!

#' Title
#'
#' @param border
#' @param operation
#' @param shift
#' @param scale
#' @param angle
#'
#' @return
#' @export
#'
#' @examples
placebo_border <- function(border = cut_off.sf, operation = c("shift", "scale", "rotate"),
                           shift = c(0, 0), scale = 1, angle = 0) {

  # TODO
  # - make the function more generic and let user decide on which fraction of the side she wants the line to end

  cat("Pay attention to CRS! If in 4326 then degrees have to be provided. For precision we would prefer a local CRS!\n")

  # first we take out the geometry and the centroid
  border_sfc <- sf::st_geometry(border)
  border_centroid_sfc <- sf::st_centroid(border_sfc)

  #----------------------------------------------------------------------
  if ("shift" %in% operation) {

    #if (st_geometry_type(border)[1] == "POINT") { # cases for the different types, but we prbly don't need

    border_sfc <- border_sfc + c(shift[1], shift[2]) # units are in deg or meters here

    #} else {
    #  cat("nothing of use provided")
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
  border_sfc
}



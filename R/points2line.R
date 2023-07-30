
#' Title
#' this is just a test function, export was removed for now
#'
#' @param borderpoints a set of points on a boundary
#' @param crs set the coordinate reference system (CRS)
#'
#' @return a line as an sf object
#'
#' @examples \dontrun{test}

points2line <- function(borderpoints, crs) {


  borderpoints <- sf::st_cast(borderpoints, "MULTIPOINT") # make multi
  # what happens if we throw in multi already? fine


  # creat lines by previous sorting and save them in the list
  #multipoints <- sf::st_multipoint(as.matrix(cbind(border$POINT_X, border$POINT_Y)), dim = "XY")
  multipoints <- sf::st_coordinates(borderpoints) # this takes the coordinates directly from the geometry of the object
  lines <- lapply(1:(nrow(multipoints) - 1), function(i) {
    sf::st_linestring(rbind(multipoints[i, ], multipoints[i+1, ]))
  })

  # now we convert it to an sf object
  lines.sf <- sf::st_sfc(lines, crs = crs)
  lines.sf <- sf::st_cast(lines.sf, "MULTILINESTRING")
  lines.sf

}

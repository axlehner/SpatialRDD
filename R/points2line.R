


#' Title
#' this is just a test function, export was removed for now
#' @param border test
#' @param crs test
#'
#' @return
#'
#' @examples test
points2line <- function(border = borderpoints, crs = projectcrs) {


  border <- st_cast(border, "MULTIPOINT") # make multi
  # what happens if we throw in multi already? fine


  # creat lines by previous sorting and save them in the list
  #multipoints <- st_multipoint(as.matrix(cbind(border$POINT_X, border$POINT_Y)), dim = "XY")
  multipoints <- st_coordinates(border) # this takes the coordinates directly from the geometry of the object
  lines <- lapply(1:(nrow(multipoints) - 1), function(i) {
    st_linestring(rbind(multipoints[i, ], multipoints[i+1, ]))
  })

  # now we convert it to an sf object
  lines.sf <- st_sfc(lines, crs = crs)
  lines.sf <- st_cast(lines.sf, "MULTILINESTRING")
  lines.sf

}

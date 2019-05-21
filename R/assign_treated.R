
#
# this function takes in the points.sf file with all observations
# and assigns the treatment dummy based on a shapefile/geopackage/sf polygon file that covers the full area of the treated region
# it returns the full points.sf data frame with one additional column that is called "treated"
# it also need the column name of the unique identifier of the points.sf file
#

#' Title
#'
#' @param data
#' @param polygon
#' @param id
#'
#' @return
#' @export
#'
#' @examples
assign_treated <- function(data.sf = points.sf, polygon.sf = polygon_treated.sf, id = "id_column") {

  # no deparse(substitute(colname)), require the string directly

  # PRECHECK IF CRS' ARE MATCHING!

  # also change id to NA by default so we can get an easy error message when not provided (otherwise might lead to confusion bc it works without providing it)


  # retrieving the id's that were treated (this function effectively subsets the whole df/sf object, we extract only one column of that)
  over_id <- st_intersection(polygon.sf, data.sf)[[id]]
  data.sf[["treated"]] <- 0
  # here we have a very clumsy way to access the column we just created, just to keep in mind that would work
  data.sf[[deparse(substitute(treated))]][data.sf[[id]] %in% over_id] <- 1
  # spit out the sf df
  return(as.factor(data.sf[["treated"]]))
}

# v 1.0 done

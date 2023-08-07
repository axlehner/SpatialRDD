

#' Let the package know which observations were treated
#'
#' Creates a vector with 0's and 1's to determine on which side of the cut-off each observation is. For this it is useful to have a polygon that fully describes the "treated area".
#' If you do not have such a polygon there is a (preliminary and patchy) way implemented in the package via \code{\link{points2line}} and \code{\link{cutoff2polygon}} that lets you go from points to line to "treated polygon" in a very crude way.
#'
#' @param data sf data frame containing point data (if you have polygons, convert first with sf::st_centroid())
#' @param polygon sf object with polygon geometry that fully describes the area(s) that contain the treated points
#' @param id string that represents the name of the column in the data that represents the unique identifier for each observation
#'
#' @return A vector of type factor with 0's and 1's. Convert with as.numeric() if you want real numbers/integers.
#' @export
#'
#' @note This is essentially a wrapper of \code{sf::st_intersection}.
#'
#' @examples
#' points_samp.sf <- sf::st_sample(polygon_full, 100) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' # add a unique ID to each observation:
#' points_samp.sf$id <- 1:nrow(points_samp.sf)
#' points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
#'
assign_treated <- function(data, polygon, id = NA) {
  # \code{\link[sf:geos_binary_ops]{st_intersection}}.
  # no deparse(substitute(colname)), require the string directly

  # PRECHECKS on the id col
  if (is.na(id)) {message("Please provide column name for unique id point layer as a string.\n")
    return()}
  if (typeof(id) != "character") {message("Column name for unique id point layer is not a string, wrap it around \" \".\n")
    return()}
  #microbenchmark::microbenchmark(any(names(data) == id), id %in% names(data))
  stopifnot("id column not found. Did you specify it correctly? (check spelling, is it a string?)"
            = any(names(data) == id))

  # CHECKS ON SF OBJECTS
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "treatment polygon is not an sf object" = inherits(polygon, "sf"),
    "CRS not matching between objects, transform them accordingly!"
    = sf::st_crs(data) == sf::st_crs(polygon)
  )

  # retrieving the id's that were treated (this function effectively subsets the whole df/sf object, we extract only one column of that)
  over_id <- sf::st_intersection(polygon, data)[[id]]
  data[["treated"]] <- 0
  # here we have a very clumsy way to access the column we just created, just to keep in mind that would work
  data[[deparse(substitute(treated))]][data[[id]] %in% over_id] <- 1
  # spit out the sf df
  return(as.factor(data[["treated"]]))
}

# v 0.1.0 done
# fixed example and made it executable by removing dontrun{}

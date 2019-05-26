


#' Let the package know which observations were treated
#'
#' Creates a vector with 0's and 1's to determine on which side of the cut-off each observation is. For this it is useful to have a polygon that fully describes the "treated area".
#' If you do not have such a polygon there is a (very preliminary and patchy) way implemented in the package via \code{\link{points2line}} and \code{\link{cutoff2polygon}} that let's you go from points to line to "treated polygon" in a very crude way.
#'
#' @param data.sf sf object containing point data
#' @param polygon.sf sf object with polygon geometry that fully describes the area(s) that contain the treated points
#' @param id string that represents the name of the column in the points object that represents the unique identifier for each observation
#'
#' @return A vector of type factor with 0's and 1's. Convert with as.numeric() if you want real numbers/integers.
#' @export
#'
#' @examples
#' points.sf$treated <- assign_treated(points.sf, polygon_treated.sf, id = "id")
#'
assign_treated <- function(data.sf = points.sf, polygon.sf = polygon_treated.sf, id = NA) {

  # no deparse(substitute(colname)), require the string directly

  # PRECHECK IF CRS' ARE MATCHING!

  if (is.na(id)) {cat("Please provide column name for unique id point layer as a string.\n")
    return()}

  # retrieving the id's that were treated (this function effectively subsets the whole df/sf object, we extract only one column of that)
  over_id <- sf::st_intersection(polygon.sf, data.sf)[[id]]
  data.sf[["treated"]] <- 0
  # here we have a very clumsy way to access the column we just created, just to keep in mind that would work
  data.sf[[deparse(substitute(treated))]][data.sf[[id]] %in% over_id] <- 1
  # spit out the sf df
  return(as.factor(data.sf[["treated"]]))
}

# v 1.0 done
# v 1 documentation donex



#' Border Segment Creation for FE-estimation
#'
#' Creates \code{n} segments of a line (the RD cut-off) and assigns the closest border segment for each observation in the sf data frame.
#' Computationally these tasks are quite demanding when the sample size is big and thus might take a few seconds to complete.
#'
#' @param data  sf data frame containing point data
#' @param cutoff the RDD border in the form of a line (preferred) or borderpoints
#' @param n the number of segments to be produced
#'
#' @return a vector with factors, each category representing one segment
#' @export
#'
#' @examples
#' points_samp.sf <- sf::st_sample(polygon_full, 100) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' points_samp.sf$segment10 <- border_segment(points_samp.sf, cut_off, 3)
#'
border_segment <- function(data, cutoff, n = 10) {

  # CHECKS ON SF OBJECTS
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "cutoff is not an sf object"            = inherits(cutoff, "sf"),
    "CRS not matching between objects, transform them accordingly!"
                                            = sf::st_crs(data) == sf::st_crs(cutoff)
  )

  # first bifurcation depending on input type (POINT or LINE)
  if (sf::st_geometry_type(cutoff)[1] == "POINT" | sf::st_geometry_type(cutoff)[1] == "MULTIPOINT") {
    #message("Drawing", n, "points out of the given set of boundarypoints.")
    # only random sampling implemented for now in st_sample
    # thus i cook up standard subsetting
    cutoff$id <- 1:nrow(cutoff) # create a unique id
    # revision after miscoding as "always start with id = 1"
    # borderpoints.sf <- cutoff[seq(1, nrow(cutoff), round(nrow(cutoff) / n, 0)), ] # subset according to this rule
    # NEW APPROACH, comp efficient with RAM efficient solution from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
    temp <- split(cutoff$id, sort(cutoff$id%%n)) # slice it in n equal parts
    temp <- as.numeric(lapply(temp,function(x){return(x[round(length(x)/2, 0)])})) # get the middle element of each slice
    borderpoints.sf <- cutoff[temp, ] # subset with the chosen boundarypoints
    borderpoints.sf$id <- 1:nrow(borderpoints.sf)
    closest <- list()
    for(i in seq_len(nrow(data))){
      closest[[i]] <- borderpoints.sf[which.min(sf::st_distance(borderpoints.sf, data[i, ])),]$id
    }
    closest <- as.factor(as.numeric(closest))
    closest

  } else { # this is when it is a line
    # this ensures that cutoffs with linegeometries that are not suited get transformed without braking the ones that are actually fine
    cutoff <- cutoff %>% sf::st_cast("LINESTRING")
    #cutoff <- cutoff %>% st_union() %>% st_as_sf() %>% st_transform(3857)
    #message("Starting to create", n, "border segments with an approximate length of", round(as.numeric(sf::st_length(cutoff))/1000/n, 0), "kilometres each.\n")
    borderpoints.sf <- sf::st_line_sample(cutoff, n)
    borderpoints.sf <- borderpoints.sf %>% sf::st_cast("POINT") # cast to POINT in order to get the rownumber right
    borderpoints.sf <- sf::st_sf(borderpoints.sf) # then we need to make it an sf data frame again
    borderpoints.sf$id <- 1:nrow(borderpoints.sf)
    closest <- list()
    for(i in seq_len(nrow(data))){
      closest[[i]] <- borderpoints.sf[which.min(sf::st_distance(borderpoints.sf, data[i, ])),]$id
    }
    closest <- as.factor(as.numeric(closest))
    closest
  }
}

# v 0.1.0 done
# suppressed print messages to console

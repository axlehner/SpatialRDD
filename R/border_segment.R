

#' Border Segment creation
#'
#' This function assigns the closest border segment for each observation in the sf object. It takes in the border, cuts it into n segments, and finally assigns each observation the relevant segment as factor. Thus allows the researcher then to carry out fixed-effects regressions. (i.e. allow for different intercepts and only exploit within segment variation).
#' Computationally these tasks are quite demand when the sample size is big and thus might take a few seconds to complete.
#'
#' @param data  sf object containing point data
#' @param cutoff the RDD border in the form of a line (preferred) or borderpoints
#' @param n the number of segments to be produced
#'
#' @return a vector with factors, each category representing one segment
#' @export
#'
#' @examples
#' points.sf$segment10 <- border_segment(points.sf, cut_off.sf, 10)
border_segment <- function(data = points.sf, cutoff = cut_off.sf, n = 10) {


  # first bifurcation depending on input type
  if (sf::st_geometry_type(cutoff)[1] == "POINT" | sf::st_geometry_type(cutoff)[1] == "MULTIPOINT") {
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
    cat("Starting to create", n, "border segments with an approximate length of", round(as.numeric(sf::st_length(cutoff))/1000/n, 0), "kilometres each.\n")
    cutoff <- cutoff %>% sf::st_cast("LINESTRING")
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

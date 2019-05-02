#'
#' This function takes in a border in the form of a polyline
#' and converts it into point data
#' these points are later used to calculate a heterogenous treatment effect alongside the border (i.e. the cut-off)
#'
#' supported formats for the line:

# need to format this so i can run the nrow(), so ideally avoid a multilinestring output
discretise_border <- function(cutoff = cut_off.sf, n = 10, random = F) {

  # do I need to prepare for other cases where the input is neither linestring nor multilinestring?
  cat("Starting to create", n, "borderpoints. Approximately every", round(as.numeric(st_length(cutoff))/1000/n, 0), "kilometres we can run an estimation then.\n")
  if (random == F) {
    cutoff <- cutoff %>% st_cast("LINESTRING")
    borderpoints.sf <- st_line_sample(cutoff, n)
  } else {
    borderpoints.sf <- st_sample(cutoff, n)
  }

  # need single points so we can loop over them afterwards
  borderpoints.sf <- borderpoints.sf %>% st_cast("POINT")
  borderpoints.sf

}

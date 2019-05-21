#'
#' This function takes in a border in the form of a polyline
#' and converts it into point data
#' these points are later used to calculate a heterogenous treatment effect alongside the border (i.e. the cut-off)
#'
#' supported formats for the line:

# need to format this so i can run the nrow(), so ideally avoid a multilinestring output
discretise_border <- function(cutoff = cut_off.sf, n = 10, random = F, range = F, ymax = NA, ymin = NA) {

  # do I need to prepare for other cases where the input is neither linestring nor multilinestring?
  # here the case for line data
  if (st_geometry_type(cutoff)[1] == "LINESTRING" | st_geometry_type(cutoff)[1] == "MULTILINESTRING") {
    cat("Starting to create", n, "borderpoints. Approximately every", round(as.numeric(st_length(cutoff))/1000/n, 0), "kilometres we can run an estimation then.\n")
    if (random == F) {
      cutoff <- cutoff %>% st_cast("LINESTRING")
      borderpoints.sf <- st_line_sample(cutoff, n)
    } else {
      borderpoints.sf <- st_sample(cutoff, n)
    }

  } else { # this is the case for points (subsetting)

    cutoff$id <- 1:length(cutoff)
    borderpoints.sf <- cutoff[seq(1, nrow(cutoff), round(nrow(cutoff) / n, 0)), ] # subset according to this rule
    cat("Starting to create", n, "borderpoints. Approximately every", round(as.numeric(st_distance(borderpoints.sf[1], borderpoints.sf[2]))/1000/n, 0), "kilometres we can run an estimation then.\n")


  }

  if (range == T) {



  }
  # need single points so we can loop over them afterwards
  borderpoints.sf <- borderpoints.sf %>% st_cast("POINT")
  borderpoints.sf

}

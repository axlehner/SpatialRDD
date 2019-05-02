#
# This is function serving the "classic" RD design by Dell (2010)
#
# it takes in the border, cuts it into n segments, and finally assigns each observation the relevant segment as factor
# this allows the user to run the regression with border segment fixed effects,
# i.e. allow for different intercepts and only exploit within segment variation
#
# supported formats for the line:


border_segment <- function(data = points.sf, cutoff = cut_off.sf, n = 10) {

  cat("Starting to create", n, "border segments with an approximate length of", round(as.numeric(st_length(cutoff))/1000/n, 0), "kilometres each.\n")

  cutoff <- cutoff %>% st_cast("LINESTRING")
  borderpoints.sf <- st_line_sample(cutoff, n)
  borderpoints.sf <- borderpoints.sf %>% st_cast("POINT") # cast to POINT in order to get the rownumber right
  borderpoints.sf <- st_sf(borderpoints.sf) # then we need to make it an sf data frame again
  borderpoints.sf$id <- 1:nrow(borderpoints.sf)
  closest <- list()
  for(i in seq_len(nrow(data))){
    closest[[i]] <- borderpoints.sf[which.min(st_distance(borderpoints.sf, data[i, ])),]$id
  }
  closest <- as.factor(as.numeric(closest))
  closest
}

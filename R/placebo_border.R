
# input should be able to be point or line
# lonlat plus utm local
# need to assign treated, then report how many treated ones flipped the status
## how? polygon (rescale equally), or with the above/under rule from stackexchange?

## also for the SpatialRD this matters quite a lot

# putput should always be a polygon so that we can assign the treated!

placebo_border <- function(border = cut_off.sf, distance = 0, buffer = T) {

  cat("Pay attention to CRS! If in 4326 then degrees have to be provided. For precision we would prefer a local CRS!\n")

  if (buffer == T) {

    placebo_polygon <- st_buffer(border, dist = distance)
    placebo_polygon

  } else {

    #if (st_geometry_type(border)[1] == "POINT") { # cases for the different types, but we prbly don't need

      border_sfc <- st_geometry(border)
      border_shift <- border_sfc + c(distance, distance) # units are in deg or meters here
      # border <- st_set_geometry(border, border_shift)
      st_crs(border_shift) <- st_crs(border)
      border_shift
    #} else {
    #  cat("nothing of use provided")
    #}


  }






  # tm_shape(cut_off.sf) + tm_lines()
  #
  # tm_sfc <- st_geometry(cut_off.sf)
  # tm_centroid_sfc <- st_centroid(tm_sfc)
  # tm_scale <- (tm_sfc - tm_centroid_sfc) * 0.7 + tm_centroid_sfc
  # #tm_scale <- tm_scale - c(2000, 0)
  # tm_scale.sf <- st_set_geometry(cut_off.sf, tm_scale)
  # tm_shape(cut_off.sf) + tm_lines() + tm_shape(tm_scale.sf) + tm_lines(col = "red")
  #
  # border_sfc <- st_geometry(cut_off.sf)
  # border_shift <- border_sfc + c(0, 2000) # units are in metres
  # border <- st_set_geometry(cut_off.sf, border_shift)
  #
  # tm_shape(cut_off.sf) + tm_lines() + tm_shape(border) + tm_lines(col = "red")

}



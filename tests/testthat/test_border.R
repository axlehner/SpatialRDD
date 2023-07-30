context("Border Tests")
library(SpatialRDD); data(cut_off, polygon_full)
# create some points
points_samp.sf    <- sf::st_sample(polygon_full, 100)
points_samp.sf    <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation


test_that("border_segment gives correct number of segments", {
  expect_equal(length(unique(border_segment(points_samp.sf, cut_off, 5))), 5)
})


test_that("discretise_border gives correct number of points", {
  expect_equal(nrow(discretise_border(cutoff = cut_off, n = 50)), 50)
})

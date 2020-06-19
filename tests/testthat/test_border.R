context("Border Tests")
library(SpatialRDD); data(cut_off.sf, polygon_full.sf)
# create some points
set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full.sf, 100)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation


test_that("border_segment gives correct number of segments", {
  expect_equal(length(unique(border_segment(points_samp.sf, cut_off.sf, 5))), 5)
})


test_that("discretise_border gives correct number of points", {
  expect_equal(nrow(discretise_border(cutoff = cut_off.sf, n = 50)), 50)
})

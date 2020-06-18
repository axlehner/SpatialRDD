
<!-- README.md is generated from README.Rmd. Please edit the latter file - rmarkdown::render('README.Rmd', output_format = 'github_document', output_file = 'README.md') -->

# Spatial[R]()DD

This repository hosts the code underlying the R package `SpatialRDD`.
The workhorse functions in a nutshell are:

  - `assign_treated()`
  - `border_segment()`
  - `discretise_border()`
  - `spatialrd()`
  - `plotspatialrd()`
  - `printspatialrd()`
  - `shift_border()`
  - `cutoff2polygon()`

The package can estimate heterogenous treatment effects alongside an RD
cutoff. Moreover it provides powerful spatial functions to carry out
placebo exercises (move borders and reassign (placebo) treatment
status). These functionalities are also useful for different empirical
identification strategies that rely on flexibly changing geographic
boundaries.

For full guidance check out the different vignettes in the vignettes
folder here on github or with

  - `vignette(spatialrdd_vignette)`
  - `vignette(shifting_borders)`

in the R console.

## Installation

``` r
install.packages("devtools")
devtools::install_github("axlehner/SpatialRDD")
```

## Quick Guide

``` r
library(SpatialRDD); data(cut_off.sf, polygon_full.sf, polygon_treated.sf)
# simulate some data
set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full.sf, 1000)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation
points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated.sf, id = "id") # assign treated
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries

points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) + points_samp.sf$education[points_samp.sf$treated == 1]
#> Warning in rnorm(NTr, mean = 0, sd = 0.1) + points_samp.sf$education[points_samp.sf$treated == : longer object length is not
#> a multiple of shorter object length
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) + points_samp.sf$education[points_samp.sf$treated == 0]
#> Warning in rnorm(NCo, mean = 0, sd = 0.1) + points_samp.sf$education[points_samp.sf$treated == : longer object length is not
#> a multiple of shorter object length
#> Warning in points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, : number of items to replace is not a multiple
#> of replacement length
```

Adding border segment for transparent fixed effect category creation:

``` r
points_samp.sf$segment5 <- border_segment(points_samp.sf, cut_off.sf, 5)
#> Starting to create 5 border segments with an approximate length of 26 kilometres each.
tm_shape(points_samp.sf) + tm_dots("segment5", size = 0.1) + tm_shape(cut_off.sf) + tm_lines()
```

![](figures/unnamed-chunk-3-1.png)<!-- -->

Create points alongside border and run GRD

``` r
borderpoints.sf <- discretise_border(cutoff = cut_off.sf, n = 50)
#> Starting to create 50 borderpoints from the given set of borderpoints. Approximately every 3 kilometres we can run an estimation then.
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10)
#> We have 1000 observations of which 215 are treated observations.
#> We are iterating over 50 Boundarypoints.
#> The dependent variable is education .
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced

#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced

#> Warning in log(fhatl): NaNs produced

#> Warning in log(fhatl): NaNs produced

#> Warning in log(fhatl): NaNs produced

#> Warning in log(fhatl): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced

#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatr): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
#> Warning in log(fhatl): NaNs produced
#> Warning in sqrt((1/(rn * bw)) * (24/5) * ((1/fhatr) + (1/fhatl))): NaNs produced
plotspatialrd(results, map = T)
```

![](figures/unnamed-chunk-4-1.png)<!-- -->

## References

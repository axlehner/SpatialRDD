
<!-- This brief doc/description is only for the github page. README.md is generated from README.Rmd. Please edit the latter file - rmarkdown::render('README.Rmd', output_format = 'github_document', output_file = 'README.md') or devtools::build_readme() -->
<!-- pre-CRAN checks: usethis::use_cran_comments(), devtools::check(remote = T), rhub::check_for_cran() # presubmission in an as_if_cran scenario, usethis::use_version(), update cran comments, FINAL devtools::submit_cran() -->

# Spatial[R]()DD

<!-- badges: start -->
<!-- some dependency does not work, I have 0 error 0 warning [![Travis build status](https://travis-ci.com/axlehner/SpatialRDD.svg?branch=master)](https://travis-ci.com/axlehner/SpatialRDD) -->

[![R-CMD-check](https://github.com/axlehner/SpatialRDD/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/axlehner/SpatialRDD/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/SpatialRDD)](https://CRAN.R-project.org/package=SpatialRDD)
<!-- badges: end -->

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

`SpatialRDD` is the first (geo-)statistical package that unifies the
geographic tasks needed for spatial RDDs with all potential parametric
and non-parametric estimation techniques that have been put forward
\[see @Lehner2023a\]. It makes it easy to understand critical
assumptions regarding bandwidths, sparse border points, and border
segment fixed effects. The package can estimate heterogeneous treatment
effects alongside an RD cutoff. Moreover it provides powerful spatial
functions to carry out placebo exercises (move borders and reassign
(placebo) treatment status). These functionalities are also useful for
different empirical identification strategies that rely on flexibly
changing geographic boundaries.

For full guidance check out the different vignettes on the [package
website](https://axlehner.github.io/SpatialRDD/articles/spatialrdd_vignette.html)
or with

- `vignette(spatialrdd_vignette)`
- `vignette(shifting_borders)`

in the R console. The functions that are presented in the latter are
potentially useful for other research designs that rely on (randomly)
shifting (many) borders.

## Installation

``` r
# From CRAN
install.packages("SpatialRDD")

# For the latest stable development version
install.packages("devtools")
devtools::install_github("axlehner/SpatialRDD") # add build_vignettes = TRUE if you want to have access to them via R, otherwise just look at the .Rmd on github in \vignettes 
```

What you need to run you own spatial RD with `SpatialRDD`:

1.  An RD boundary as a single line (the simplest way is to cut this by
    hand in e.g. ArcGIS or QGIS by just splitting a line off a polygon
    for example - also make sure to merge all features together in case
    there are multiple left, this will be important and prevent annoying
    troubles later on).
2.  The data frame containing the columns with x- and y-coordinates.
    Read in with `read.csv()` or with `readstata13::read.dta13()`. Make
    them an sf object with
    `st_as_sf(data, coords = c("x", "y"), crs = 4326)` if you have
    longitude/latitude as coordinates (which is reflected by the 4326
    EPSG). Note: In case your data comes as a shapefile/geopackage/etc.
    directly, just read it in with `st_read("path/to/file.shp")`. If
    these are polygons it is advised to work with the centroids
    straightaway (extract with `st_centroid()`). If you need zonal
    statistics (on elevation/ruggednes etc.) for e.g. checking
    identifying assumptions, do these before converting to centroids.
3.  Ideally also a polygon that covers the treated areas (this could be
    created within the package with `cutoff2polygon` though).
4.  Be careful when you choose your coordinate reference system (CRS), I
    suggest to transform your data into a projected CRS and work on the
    euclidean plane (instead of on the sphere with angles, that is, with
    longitude and latitude). You can do this with `sf::st_transform()`.
    See the reference below for more details.

You could verify the “geographic validity” of your objects with
e.g. `mapview::mapview()`.

## Quick Guide

Adding border segment for transparent fixed effect category creation:

``` r
points_samp.sf$segment5 <- border_segment(points_samp.sf, cut_off, 5)
tm_shape(points_samp.sf) + tm_dots("segment5", size = 0.1) + tm_shape(cut_off) + tm_lines()
```

![](man/figures/README-border_segment-1.png)<!-- -->

If you want, you can create boundarypoints alongside the border to,
inter alia, explore heterogeneity:

``` r
borderpoints.sf <- discretise_border(cutoff = cut_off, n = 50)
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 30)
plotspatialrd(results, map = T)
```

![](man/figures/README-grd-1.png)<!-- -->

## References

Lehner (2024) [A Note On Spatial Regression Discontinuity
Designs](https://lehner.xyz/pdf/Lehner_SpatialRDDnote.pdf), mimeograph.

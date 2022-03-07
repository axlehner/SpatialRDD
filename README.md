
<!-- This brief doc/description is only for the github page. README.md is generated from README.Rmd. Please edit the latter file - rmarkdown::render('README.Rmd', output_format = 'github_document', output_file = 'README.md') -->
<!-- pre-CRAN checks: usethis::use_cran_comments(), devtools::check(remote = T), rhub::check_for_cran() # presubmission in an as_if_cran scenario, usethis::use_version(), update cran comments, FINAL devtools::submit_cran() -->

# Spatial[R]()DD

<!-- badges: start -->
<!-- some dependency does not work, I have 0 error 0 warning [![Travis build status](https://travis-ci.com/axlehner/SpatialRDD.svg?branch=master)](https://travis-ci.com/axlehner/SpatialRDD) -->
<!-- badges: end -->

This repository hosts the code underlying the R package `SpatialRDD`.
The workhorse functions in a nutshell are:

-   `assign_treated()`
-   `border_segment()`
-   `discretise_border()`
-   `spatialrd()`
-   `plotspatialrd()`
-   `printspatialrd()`
-   `shift_border()`
-   `cutoff2polygon()`

The package can estimate heterogenous treatment effects alongside an RD
cutoff. Moreover it provides powerful spatial functions to carry out
placebo exercises (move borders and reassign (placebo) treatment
status). These functionalities are also useful for different empirical
identification strategies that rely on flexibly changing geographic
boundaries.

For full guidance check out the different vignettes in the vignettes
folder here on github or with

-   `vignette(spatialrdd_vignette)`
-   `vignette(shifting_borders)`

in the R console. The functions that are presented in the latter are
potentially useful for other research designs that rely on (randomly)
shifting (many) borders.

## Installation

``` r
install.packages("devtools")
devtools::install_github("axlehner/SpatialRDD") # add build_vignettes = TRUE if you want to have access to them via R, otherwise just look at the .Rmd on github in \vignettes 
```

What you need to run you own spatial RD with `SpatialRDD`:

1.  An RD boundary as a single line (the simplest way is to cut this by
    hand in e.g. ArcGIS or QGIS by just splitting a line off a polygon
    for example - also make sure to merge all features together in case
    there are multiple left, this will be important and prevent annoying
    troubles later on)
2.  The data frame containing the columns with x- and y-coordinates.
    Read in with `read.csv()` or with `readstata13::read.dta13()`. Make
    them an sf object with
    `st_as_sf(data, coords = c("x", "y"), crs = 4326)` if you have
    longitude/latitude as coordinates (which is reflected by the 4326
    EPSG). If this is the case use `st_transform()` on all your objects
    into a local UTM projection (not necessary but recommended for
    several reasons). Note: In case your data comes as a
    shapefile/geopackage/etc. directly, just read it in with
    `st_read("path/to/file.shp")`. If these are polygons it is advised
    to work with the centroids straightaway (extract with
    `st_centroid()`). If you need zonal statistics (on
    elevation/ruggednes etc.) for e.g. checking identifying assumptions,
    do these before converting to centroids.
3.  Ideally also a polygon that covers the treated areas (this could be
    created within the package with `cutoff2polygon` though)

You could verify the “geographic validity” of your objects with
e.g. `mapview::mapview()`.

## Quick Guide

Adding border segment for transparent fixed effect category creation
(for the non-parametric specification that is just OLS with `lm()` or
`lfe::felm()`):

``` r
points_samp.sf$segment5 <- border_segment(points_samp.sf, cut_off.sf, 5)
#> Starting to create 5 border segments with an approximate length of 26 kilometres each.
tm_shape(points_samp.sf) + tm_dots("segment5", size = 0.1) + tm_shape(cut_off.sf) + tm_lines()
```

![](man/figures/README-border_segment-1.png)<!-- -->

Create points alongside border and run GRD to explore heterogeneity:

``` r
borderpoints.sf <- discretise_border(cutoff = cut_off.sf, n = 50)
#> Starting to create 50 borderpoints from the given set of borderpoints. Approximately every 3 kilometres we can run an estimation then.
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10)
plotspatialrd(results, map = T)
```

![](man/figures/README-grd-1.png)<!-- -->

## References

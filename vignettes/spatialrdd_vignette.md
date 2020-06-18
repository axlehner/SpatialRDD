---
title: "SpatialRDD: get started"
author: "Alexander Lehner"
date: "2020-06-18"
output: 
  rmarkdown::html_vignette:
    keep_md: true 
    toc: true
    #toc_float: true
    #number_sections: true
    df_print: paged #does the html table thing
    #theme: simplex
#self_contained: no
vignette: >
  %\VignetteIndexEntry{SpatialRDD: get started}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




Within the last years spatial versions of Regression Discontinuity Designs (RDDs) have increased tremendously in popularity in the social sciences. Executing spatial RDDs, especially the many required robustness- and sensitivity checks, in practice is quite cumbersome though. It requires knowledge of both statistical programming and how to handle and work with geographic objects (points, lines, polygons). Practitioners typically carry out these GIS tasks in a GUI like ArcGIS or QGIS and then manually export these data into the statistical environment of their choice and then carry out the analysis in an "aspatial way". This is sub-optimal for several reasons:
* it is very time consuming and cumbersome
* it is prone to errors
* it is not reproducible (which is especially annoying for oneself when it comes to e.g. revisions)
* it is not very flexible and makes it harder to understand our own data

`SpatialRDD` is the first (geo-)statistical package of its kind that unifies the geographic tasks that are needed for spatial RDDs with all potential parametric and non-parametric estimation techniques that have been put forward (see Lehner2019). It makes it very easy to understand the critical assumptions (in terms of bandwith, sparse border points)
Furthermore, the flexibility of the `shift_border` function makes it attractive for all sorts of identification strategies that rely on shifting placebo borders.

Geographic objects are treated as [simple features](https://en.wikipedia.org/wiki/Simple_Features) throughout, making heavy use of the novel `sf` package by Edzer Pebesma which revolutionised spatial data analysis in **R** and is bound to supersede the older and less versatile `sp` package.  
`SpatialRDD` facilitates analysis inter alia because it contains all necessary functions to automatise otherwise very tedious tasks that are tipically carried out in GUIs such as QGIS or ArcGIS. Due to the fact that these GIS interfaces are not able to carry out appropriate statistical analysis, the researcher is tipically forced to migrate the obtained results to statistical software. This makes reproducibility difficult and most importantly is a very inefficient workflow.  
`SpatialRDD` unifies everything in one language and e.g. has the necessary functions to check for different bandwiths, shift placebo boundaries, do all necessary distance calculations, assign treated/non-treated dummies, and flexibly assign border segment fixed effects while keeping the units of observations at their proper position in space and allowing the researcher to visualise every intermediate step with mapplots. For the latter we will mostly rely on the flexible and computationally very efficient `tmap` package, while also `ggplot2` is used at times.  
For the purpose of illustration, this vignette uses simulated data on real boundaries/polygons and guides the user through every necessary step in order to carry out a Geographic RDD estimation. At the appropriate points, we will also make remarks on technical caveats and issues that have been pointed out in the literature and give suggestions to improve these designs.   
The workhorse functions of `SpatialRDD` in a nutshell are:

* `assign_treated()`
* `border_segment()`
* `discretise_border()`
* `spatialrd()`
* `plotspatialrd()`
* `printspatialrd()`
* `shift_border()`
* `cutoff2polygon()`

and they are going to be introduced here in precisely this order.

# Some words of caution

all the projections system importance plus the story mit GADM is dangerous (visualise with leaflet odersowas auf openstreetmap)

# Setup and Propaedeutics

Throughout the vignette we will use the geographic boundaries on Goa, India, from Lehner (2019). The data, included in the package, contains

1. a line called `cut_off.sf` which describes the spatial discontinuity
2. a polygon that defines the "treated" area
3. a polygon that defines the full study area (which is going to be useful as this defines the bounding box)

For your own RD design you need 1. in the form of either a line (as a single feature) or a finely spaced set of points on your border. Furthermore you need the polygon that specifies the treated area, and, of course, the dataset that contains your units of observation.


```r
library(SpatialRDD)
library(dplyr) # more intuitive data wrangling
library(stargazer) # easy way to make model output look more appealing (R-inline, html, or latex)
library(sf)
```

These come in the EPSG EPSG:32643 projection system, which is a "localised" [UTM](https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system) coordinate system. These are generally prefereable for exercises like a Spatial RDD, as they are more precise and also allow us to work in metres (the "classic" longitude/latitude CRS, EPSG 4326, works in degrees). If your study area is small, you should reproject your data into the CRS of the according UTM zone (simply use `st_transform()`). To verify the units of our CRS we could simply run `st_crs(cut_off.sf)$units`.

All the spatial objects are of class `sf` from the [sf package](https://CRAN.R-project.org/package=sf). This means they are just a `data.frame` with a special column that contains a geometry for each row. The big advantage is, no matter if you prefer base R, `dplyr`, or any other way to handle and wrangle your data, the `sf` object can be treated just like a standard `data.frame`. The one single step that transforms these spatial objects back to a standard `data.frame` is just dropping the geometry column with

```r
st_geometry(any.sf.object) <- NULL
```
or alternatively

```r
st_set_geometry(any.sf.object, NULL)
```

If you import geospatial data in a different format, say the common shapefile (`*.shp`) - which is NOT preferrable [see why here](http://switchfromshapefile.org/), or as a geopackage, it is fairly straightforward to convert it:


```r
mydata.sf <- st_read("path/to/file.shp")
```

In case your data is saved as a .csv (if in stata file format, check the `foreign` and `readstata13` package) you just have to tell `sf` in which columns the X- and Y-coordinates are saved, and it will convert it into a spatial object:

```r
mydata.sf <- st_as_sf(loaded_file, coords = c("longitude", "latitude"), crs = projcrs) 
# just the EPSG as an integer or a proj4string of the desired CRS
```

For more thorough information I suggest to consult the documentation and vignettes of the `sf` package.

## Inspecting the Study Area & simulating Data

```r
data(cut_off.sf, polygon_full.sf, polygon_treated.sf)
library(tmap)
tm_shape(polygon_full.sf) + tm_polygons() + 
  tm_shape(polygon_treated.sf) + tm_polygons(col = "grey") + 
  tm_shape(cut_off.sf) + tm_lines(col = "red")
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Above we see the simple map, visualising the "treated polygon" in a darker grey, and the `tmap` syntax that produced it.

Let's simulate some random points within the polygon that describes the full study area:

```r
set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full.sf, 1000)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation
# visualise results together with the line that represents our RDD cut-off
tm_shape(points_samp.sf) + tm_dots() + tm_shape(cut_off.sf) + tm_lines(col = "red")
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Assign Treatment

Now we use the first function of the `SpatialRDD` package. `assign_treated()` in essence just does a spatial intersection and returns a column vector that contains `0` or `1`, depending on whether the observation is inside or outside the treatment area. Thus, we just add it as a new column to the points object. The function requires the name of the points object, the name of the polygon that defines the treated area, and the id that uniquely identifies each observation in the points object:


```r
points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated.sf, id = "id")
tm_shape(points_samp.sf) + tm_dots("treated", palette = "Set1") + tm_shape(cut_off.sf) + tm_lines(col = "red")
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

As a next step we add an outcome of interest that we are going to use as dependent variable in our Spatial Regression Discontinuity Design. Let's call this variable `education`, measuring the literacy rate that ranges from 0 to 1 (0%, meaning everyone illiterate to 100%, meaning everyone in the population can read and write). We assume that the units, call them villages, in the "treated" polygon have on average a higher literacy rate because they received some sort of "treatment". Let's just assume aliens dropped (better) schools in all of these villages, but NOT in any of the outside villages, and everything else is constant and identical across the two territories. 


```r
# first we define a variable for the number of "treated" and control which makes the code more readable in the future
NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
# the treated areas get a 10 percentage point higher literacy rate
points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
# and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
# we draw from a normal with mean 0 and a standard devation of 0.1
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 1]
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 0]

# let's also add a placebo outcome that has no jump
points_samp.sf$placebo <- rnorm(nrow(points_samp.sf), mean = 1, sd = .25)

# visualisation split up by groups
library(ggplot2)
ggplot(points_samp.sf, aes(x = education)) + geom_histogram(binwidth = .01) + facet_grid(treated ~ .)
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

From the above histograms we can see that we were successful in creating different group means. This is also confirmed by the simple univariate regression of $y_i = \alpha + \beta\ \unicode{x1D7D9}(treated)_i + \varepsilon_i$:


```r
list(lm(education ~ treated, data = points_samp.sf),
     lm(placebo   ~ treated, data = points_samp.sf)) %>% stargazer(type = "text")
#> 
#> ===========================================================
#>                                    Dependent variable:     
#>                                ----------------------------
#>                                   education      placebo   
#>                                      (1)           (2)     
#> -----------------------------------------------------------
#> treated1                          0.100***        0.035*   
#>                                    (0.008)       (0.020)   
#>                                                            
#> Constant                          0.600***       0.990***  
#>                                    (0.004)       (0.009)   
#>                                                            
#> -----------------------------------------------------------
#> Observations                        1,000         1,000    
#> R2                                  0.160         0.003    
#> Adjusted R2                         0.160         0.002    
#> Residual Std. Error (df = 998)      0.100         0.260    
#> F Statistic (df = 1; 998)        186.000***       3.100*   
#> ===========================================================
#> Note:                           *p<0.1; **p<0.05; ***p<0.01
```
where the intercept tells us that the average in the non-treated areas is 0.6 and treated villages have on average 0.1 more education (10 percentage points).

### Distance to Cut-off

The next essential step before we start to do proper Spatial RDD analysis, is to determine how far each of these points is away from the cut-off. Here we just make use of a function from `sf` called `st_distance()` that returns a vector with units (that we have to convert to real numbers by `as.numeric()`):


```r
points_samp.sf$dist2cutoff <- as.numeric(sf::st_distance(points_samp.sf, cut_off.sf))
```

This allows us now to investigate villages only within a certain range, say 3 kilometres, around our "discontinuity":

```r
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) + 
  tm_shape(cut_off.sf) + tm_lines()
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

And to run the univariate regression from above also just within a bandwith (this specification is already starting to resemble the RDD idea of Dell 2010). As we know the exact data generating process (no "spatial gradient" but a rather uniform assignment), it is obvious to us that this of course leaves the point estimate essentially unchanged:


```r
lm(education ~ treated, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) %>% stargazer(type = "text")
#> 
#> ===============================================
#>                         Dependent variable:    
#>                     ---------------------------
#>                              education         
#> -----------------------------------------------
#> treated1                     0.090***          
#>                               (0.015)          
#>                                                
#> Constant                     0.610***          
#>                               (0.011)          
#>                                                
#> -----------------------------------------------
#> Observations                    159            
#> R2                             0.190           
#> Adjusted R2                    0.180           
#> Residual Std. Error      0.095 (df = 157)      
#> F Statistic           36.000*** (df = 1; 157)  
#> ===============================================
#> Note:               *p<0.1; **p<0.05; ***p<0.01
```


# Carrying out Spatial RDD estimation

Now we go step by step through all potential (parametric and non-parametric) ways in which one could obtain point estimates for Spatial RDD's (see e.g. Lehner2019 for details).


## Naive Distance

For the "naive" estimation (KeeleTitiunik2015), meaning that the spatial dimension is essentially disregarded, we first define a variable `distrunning` that makes the distances in the treated areas negative so that our 2-dimensional cut-off is then at 0.


```r
points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
# give the non-treated one's a negative score
points_samp.sf$distrunning[points_samp.sf$treated == 0] <- -1 * points_samp.sf$distrunning[points_samp.sf$treated == 0]
ggplot(data = points_samp.sf, aes(x = distrunning, y = education)) + geom_point() + geom_vline(xintercept = 0, col = "red")
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

The point estimate of the "classic" non-parametric local linear regression, carried out with the `rdrobust` package, then looks like this:


```r
library(rdrobust)
summary(rdrobust(points_samp.sf$education, points_samp.sf$distrunning, c = 0))
#> Call: rdrobust
#> 
#> Number of Obs.                 1000
#> BW type                       mserd
#> Kernel                   Triangular
#> VCE method                       NN
#> 
#> Number of Obs.                 785         215
#> Eff. Number of Obs.            127         100
#> Order est. (p)                   1           1
#> Order bias  (q)                  2           2
#> BW est. (h)               4248.546    4248.546
#> BW bias (b)               6601.220    6601.220
#> rho (h/b)                    0.644       0.644
#> Unique Obs.                    785         215
#> 
#> =============================================================================
#>         Method     Coef. Std. Err.         z     P>|z|      [ 95% C.I. ]       
#> =============================================================================
#>   Conventional     0.116     0.025     4.580     0.000     [0.066 , 0.166]     
#>         Robust         -         -     4.089     0.000     [0.064 , 0.181]     
#> =============================================================================
```
and the according visualisation with data driven bin-width selection:

```r
rdplot(points_samp.sf$education, points_samp.sf$distrunning, c = 0, ci = 95, 
       kernel = "triangular", y.label = "education", x.label = "distance to border")
```

<img src="/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-16-1.png" width="\textwidth" />

For RDD estimation in **R** in general there are currently three packages flying around: `RDD`, `rddtools`, and `rddapp` (building on the `RDD`); whereby the latter seems to be the most up-to-date and comprehensive one (as it draws on previous work that others did).  
`rddapp` estimates various specifications (does not do robust inference though)


```r
library(rddapp)
summary(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"))
```

And it gives several possibilities of visualising classic RDDs. Here we arbitrarily pick one parametric and one non-parametric estimate, including confidence intervals, and manually chosen binsizes:


```r
plot(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"), fit_line = c("quadratic", "optimal"), bin_n = 50)
```

## Parametric Specifications

This method, popularised by Dell2010 in her JMP on the Peruvian Mining Mita, examines only observations within a certain distance around the border by using a (semi-)parametric approach. From the point of view of which additional "spatial technicalities" are needed, it essentially only boils down to the introduction of border segments. These are then used to apply a "within estimator" to allow for different intercepts for each of those segment categories in order to, inter alia, alleviate the omitted variable problem. As an alternative to this fixed-effects approach we might as well throw a set of dummies for each of the segments in the regression. The regression coefficient of interest then gives a weighted average over all segments. On top of that we might also be interested in the coefficient of each segment to infer something about potential heterogeneity alongside our regression discontinuity.   
The (computationally a bit demanding) function `border_segment()` only needs the points layer and the cut-off as input (preferrably as line, but also an input in the form of boundarypoint works). The last parameter of the function lets us determine how many segments we want. As with the `assign_treated()` function, the output is a vector of factors.


```r
points_samp.sf$segment10 <- border_segment(points_samp.sf, cut_off.sf, 10)
#> Starting to create 10 border segments with an approximate length of 13 kilometres each.
points_samp.sf$segment15 <- border_segment(points_samp.sf, cut_off.sf, 15)
#> Starting to create 15 border segments with an approximate length of 9 kilometres each.
tm_shape(points_samp.sf) + tm_dots("segment10", size = 0.1) + tm_shape(cut_off.sf) + tm_lines()
tm_shape(points_samp.sf) + tm_dots("segment15", size = 0.1) + tm_shape(cut_off.sf) + tm_lines()
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-19-1.png)![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-19-2.png)

It is worth noting that the researcher has to pay attention to how the fixed effects are assigned. It could, e.g. due to odd bendings of the cut-off, be the case that for some segment only one side actually gets assigned point. These situations are undesireable for estimation. It is thus paramount to always plot the fixed-effect categories on a map!
The `border_segment()` already gives the researcher a feeling for how meaningful the choice for the number of segments was. In the above example we have a segment for every 13 kilometres, which seems not too unreasonable. We could see however, that some of the FE categories contain verly little observations which is not very desireable for several reasons.  
In the following example we thus choose less borderpoints, leading to more observations on each side of the border for every segment and thus to more meaningful point estimates:


```r
points_samp.sf$segment5 <- border_segment(points_samp.sf, cut_off.sf, 5)
#> Starting to create 5 border segments with an approximate length of 26 kilometres each.
tm_shape(points_samp.sf) + tm_dots("segment5", size = 0.1) + tm_shape(cut_off.sf) + tm_lines()
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

Simple OLS estimates, using the segments that we just obtained as categories for our fixed effects, show these differences:


```r
library(lfe)
#> Loading required package: Matrix
list(felm(education ~ treated | factor(segment15) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]),
felm(education ~ treated | factor(segment5) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ])
) %>% stargazer(type = "text")
#> 
#> =====================================================
#>                            Dependent variable:       
#>                     ---------------------------------
#>                                 education            
#>                           (1)              (2)       
#> -----------------------------------------------------
#> treated1                0.094***         0.089***    
#>                         (0.016)          (0.015)     
#>                                                      
#> -----------------------------------------------------
#> Observations              159              159       
#> R2                       0.280            0.250      
#> Adjusted R2              0.200            0.220      
#> Residual Std. Error 0.094 (df = 143) 0.092 (df = 153)
#> =====================================================
#> Note:                     *p<0.1; **p<0.05; ***p<0.01
```

The confidence intervals of both point estimates are overlapping, yet, one can see that the fixed effects choice can have a substantial impact.
We obtain a point estimate that is (unsurprisingly, as we have a data generating process that is very uniform across space) very similar to the one we obtained from the simple OLS regression from the beginning. As compared to the "classic RD" point estimate that we obtained from the non-parametric local linear regression from the `rdrobust` package, the point estimate from our fixed effects regression is a bit more conservative. But from eyeballing we can determine that the average effect lies somewhere around 0.1, meaning that the literacy rate is 10 percentage points higher in the treated areas. Exactly the way we designed our simulated data.   
In the "full" polynomial specification by Dell2010 we would be required to also control for the position in space via a flexible polynomial function in longitude and latitude.


## GRD

Finally we move towards a fully fledged Geographic Regression Discontinuity (GRD) design (KeeleTitiunik2015). The function `spatialrd()` incorporates the RD estimation with two running variables, but also allows to carry out the estimation on each boundarypoint ("GRDDseries") with just one line of code. This allows us to visualise the treatment effect at multiple points of the cut-off and thus infer something about the potential heterogeneity of the effect. Or, most importantly, to assess the robustness of the GRD itself.   
A future version of `SpatialRDD` will also incorporate the "Optimized RDD" approach by ImbensWager2019.   

First of all we have to cut the border into equally spaced segments. For each of these segments, or rather boundarypoints, we will later obtain a point estimate. The `discretise_border()` function just requires the sf object that represent the cut-off (polyline preferred but also points possible) and the number of desired boundarypoints:


```r
borderpoints.sf <- discretise_border(cutoff = cut_off.sf, n = 50)
#> Starting to create 50 borderpoints from the given set of borderpoints. Approximately every 3 kilometres we can run an estimation then.
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) +
  tm_shape(cut_off.sf) + tm_lines() +
  tm_shape(borderpoints.sf) + tm_symbols(shape = 4, size = .3)
#> Legend labels were too wide. The labels have been resized to 0.66, 0.66, 0.66, 0.66, 0.66. Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

for plotting just a results table, it would be preferrable to choose just a `data.frame` as output (spatial.object = F). 

```r
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10, spatial.object = F)
#> We have 1000 observations of which 215 are treated observations.
#> We are iterating over 50 Boundarypoints.
#> The dependent variable is education .
knitr::kable(results)
```



 Point   Estimate   pvalC   pvalR   Ntr   Nco     bw   CI_Conv_l   CI_Conv_u   CI_Rob_l   CI_Rob_u
------  ---------  ------  ------  ----  ----  -----  ----------  ----------  ---------  ---------
     1       0.12    0.02    0.05    53    55   14.2        0.02        0.21       0.00       0.24
     2       0.14    0.04    0.07    71    57   15.2        0.00        0.27      -0.01       0.30
     3       0.11    0.02    0.06   103    84   19.9        0.02        0.21       0.00       0.24
     4       0.08    0.11    0.22   100    65   16.0       -0.02        0.17      -0.05       0.20
     5       0.08    0.11    0.22   104    73   17.3       -0.02        0.17      -0.04       0.19
     6       0.07    0.09    0.22   112    84   20.9       -0.01        0.15      -0.04       0.16
     7       0.06    0.08    0.20   108    62   18.9       -0.01        0.13      -0.03       0.13
     8       0.07    0.10    0.18    93    46   17.1       -0.01        0.15      -0.03       0.16
     9       0.11    0.09    0.11    77    33   14.1       -0.02        0.24      -0.03       0.27
    10       0.13    0.04    0.05    69    32   12.3        0.01        0.26       0.00       0.30
    11       0.18    0.01    0.02    56    32   11.1        0.04        0.32       0.03       0.35
    12       0.25    0.03    0.03    40    22    9.6        0.02        0.48       0.02       0.53
    13       0.32    0.02    0.02    29    17    8.2        0.04        0.60       0.05       0.65
    14       0.20    0.01    0.01    33    32    9.5        0.06        0.35       0.06       0.40
    15       0.15    0.01    0.01    24    30    8.8        0.03        0.27       0.04       0.32
    16       0.16    0.00    0.00    52    86   13.7        0.06        0.25       0.06       0.28
    17       0.18    0.00    0.00    30    45   10.7        0.07        0.29       0.07       0.33
    18       0.15    0.01    0.02    56    98   14.5        0.04        0.26       0.02       0.29
    19       0.11    0.02    0.09    96   102   16.4        0.01        0.20      -0.01       0.21
    20       0.10    0.08    0.16    85    56   13.9       -0.01        0.21      -0.04       0.23
    21       0.08    0.11    0.22    80    39   13.2       -0.02        0.19      -0.05       0.20
    22       0.07    0.21    0.35    73    53   13.3       -0.04        0.19      -0.07       0.20
    23       0.10    0.04    0.12    96    52   14.1        0.00        0.20      -0.02       0.21
    24       0.10    0.05    0.14   103    60   14.5        0.00        0.21      -0.03       0.23
    25       0.10    0.10    0.23    83    57   13.6       -0.02        0.22      -0.06       0.24
    26       0.06    0.43    0.64    55    49   11.7       -0.09        0.20      -0.13       0.22
    27       0.05    0.56    0.80    37    38    9.6       -0.11        0.21      -0.17       0.22
    30       0.01    0.88    0.95    32    34    9.2       -0.17        0.19      -0.22       0.21
    31       0.08    0.20    0.35    65    61   12.8       -0.04        0.19      -0.08       0.21
    32       0.11    0.07    0.14    47    39   10.6       -0.01        0.22      -0.03       0.24
    33       0.10    0.01    0.04   102    68   14.9        0.02        0.17       0.00       0.20
    34       0.10    0.01    0.02   109    63   14.8        0.03        0.18       0.02       0.20
    35       0.11    0.01    0.02    53    47   11.3        0.02        0.19       0.02       0.22
    36       0.17    0.00    0.00    24    25    8.6        0.11        0.23       0.12       0.25
    37       0.15    0.00    0.01    38    21    8.6        0.05        0.26       0.05       0.29
    38       0.15    0.01    0.02   108    44   13.4        0.04        0.25       0.02       0.29
    39       0.13    0.01    0.02   167    64   16.5        0.03        0.22       0.03       0.26
    40       0.14    0.01    0.01   137    62   15.1        0.04        0.24       0.04       0.28
    41       0.18    0.00    0.00    61    45   11.1        0.06        0.30       0.07       0.34
    42       0.15    0.01    0.01    57    50   10.9        0.03        0.26       0.04       0.30
    43       0.13    0.01    0.01   152    64   15.5        0.04        0.22       0.04       0.25
    44       0.11    0.04    0.05    99    60   13.3        0.01        0.22       0.00       0.26
    45       0.10    0.04    0.05   111    59   13.8        0.01        0.20       0.00       0.23
    46       0.11    0.09    0.10   112    52   13.4       -0.02        0.23      -0.02       0.28
    47       0.08    0.12    0.13   130    60   15.5       -0.02        0.18      -0.03       0.21
    48       0.06    0.12    0.18   233    75   23.0       -0.02        0.15      -0.03       0.18
    49       0.06    0.17    0.23   162    67   19.6       -0.03        0.15      -0.04       0.17
    50       0.05    0.24    0.35   112    60   17.6       -0.03        0.12      -0.05       0.14

The average treatment effect is given by taking the mean of all point estimates. Running `mean(results$Estimate)` this gives 0.12, which is exactly how we designed our DGP. For the plotting of the *GRDDseries* and a visualisation in space of each point estimate we need to have a spatial object. All this is incorporated in the `plotspatialrd()` function.


```r
results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated", minobs = 10)
#> We have 1000 observations of which 215 are treated observations.
#> We are iterating over 50 Boundarypoints.
#> The dependent variable is education .
plotspatialrd(results, map = T)
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Or just the *GRDDseries* without the map.


```r
plotspatialrd(results, map = F)
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


# Robustness

In Spatial Regression Discontinuity exercises, the researcher usually also has to show that the results are robust towards different specifications and parameters. Also in this respect the `SpatialRDD` package offers a lot of capabilities that are time saving and make replicability very easy. This toolbox for shifting and moving around borders and afterwards assigning (placebo) treatment status again is in fact so potent, that it is of use in many other research design settings outside of geographic RDs. In this vignette we will just see the basic intuiton. For more details on all the options check out the separate vignette `shifting_borders`.

## Placebo Borders

Here we are going to apply a standard tool that we got to know in linear algebra 1 classes: an affine transformation of the type $f(x) = x\mathbf{A}+b$, where the matrix $\mathbf{A}$ is the projection matrix to shift, (re-)scale, or rotate the border. For simplicity we now only apply a shift by 3000 metres in both the x and y coordinates of the border.


```r
placebocut_off.1 <- shift_border(cut_off.sf, operation = "shift", shift = c(3000, 3000))
#> Pay attention to CRS! If you work in lon/lat then degrees have to be provided. Local UTM CRS is preferable!
placeboborderpoints.1 <- discretise_border(cutoff = placebocut_off.1, n = 50)
#> Starting to create 50 borderpoints from the given set of borderpoints. Approximately every 3 kilometres we can run an estimation then.
tm_shape(points_samp.sf) + tm_dots("treated", palette = "Set1")  + tm_shape(placeboborderpoints.1) + tm_symbols(shape = 4, size = .3) + tm_shape(placebocut_off.1) + tm_lines()
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

After the border shift we now have to re-assign the new treatment status in order to carry out regressions. For that matter we create new polygons from scratch with the `cutoff2polygons()` function. The logic of this function is not very intuitive at first, but the vignette on border shifting will clarify that. In our case we do not have to go around corners with the counterfactual polygon because both ends of the cutoff go towards the west. Just make sure that the endpoints are chosen in a way so that all observations that should be in the "placebo treated" group are also actually inside this resulting polygon.



```r
placebo.poly.1 <- cutoff2polygon(data = points_samp.sf, cutoff = placebocut_off.1, orientation = c("west", "west"), corners = 0, endpoints = c(.8, .2))
#> 
#>  No corners selected, thus both extensions will end in the same side.

tm_shape(placebo.poly.1) + tm_polygons(alpha = .3) #+ tm_shape(polySideDown) + tm_polygons(alpha = .3)
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

Finally we have to use the `assign_treated()` function from the beginning of the vignette again:


```r
points_samp.sf$treated1 <- assign_treated(data = points_samp.sf, polygon = placebo.poly.1, id = "id")
sum(points_samp.sf$treated == 0 & points_samp.sf$treated1 == 1) # number of villages that switched treatment status
#> [1] 60
tm_shape(points_samp.sf) + tm_dots("treated1", palette = "Set1")  + tm_shape(placeboborderpoints.1) + tm_symbols(shape = 4, size = .3) + tm_shape(placebocut_off.1) + tm_lines()
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

After plotting the points again, we can visually infer that the right villages got assign the "treated" dummy. Further we can compute the number of villages that change their status. This helps us to decide whether the bordershift was big enough (if e.g. only a handful of observations switched, then we would expect this to have little to no impact on our point estimates and thus would dub such a robustness exercise as not very meaningful).   
In this case 60 villages changed. Given the initial number of treated observations, this seems a change of a big enough magnitude and thus a meaningful robustness exercise.   

### Robustness GRD

Finally we do the exact same exercise from above again and run the nonparametric specification on many boundary points to approximate a countinous treatment effect. The series is fluctuating around 0 and has not a single significant estimate and it is thus save to conclude that the methodology works.


```r
results1 <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = placeboborderpoints.1, treated = "treated1", minobs = 10)
#> We have 1000 observations of which 271 are treated observations.
#> We are iterating over 50 Boundarypoints.
#> The dependent variable is education .
plotspatialrd(results1, map = T)
```

![](/Users/crunchbangax/Documents/Synched/Uni/Work/2019/SpatialRDD/vignettes/spatialrdd_vignette_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

### Robustness Polynomial Specification

Finally we also run our placebo exercise with the parametric specification. Unfortunately OLS with fixed effects is not as sensitive when it comes to detecting the border shift. The coefficient is still borderline significant. In this case we should have moved the border 1 or 2 kilometres farther to make it insignificant.


```r
points_samp.sf$segment.1.5 <- border_segment(points_samp.sf, placebocut_off.1, 5) # assigning new segments based on now cutoff
#> Starting to create 5 border segments with an approximate length of 26 kilometres each.
points_samp.sf$dist2cutoff1 <- as.numeric(sf::st_distance(points_samp.sf, placebocut_off.1)) # recompute distance to new placebo cutoff

list(
  lm(education ~ treated1, data = points_samp.sf[points_samp.sf$dist2cutoff1 < 3000, ]),
  lfe::felm(education ~ treated1 | factor(segment.1.5) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff1 < 3000, ])
) %>% stargazer(type = "text")
#> 
#> ========================================================
#>                             Dependent variable:         
#>                     ------------------------------------
#>                                  education              
#>                             OLS               felm      
#>                             (1)               (2)       
#> --------------------------------------------------------
#> treated11                  0.021             0.018      
#>                           (0.015)           (0.015)     
#>                                                         
#> Constant                 0.620***                       
#>                           (0.010)                       
#>                                                         
#> --------------------------------------------------------
#> Observations                177               177       
#> R2                         0.011             0.032      
#> Adjusted R2                0.005             0.004      
#> Residual Std. Error  0.100 (df = 175)   0.100 (df = 171)
#> F Statistic         1.900 (df = 1; 175)                 
#> ========================================================
#> Note:                        *p<0.1; **p<0.05; ***p<0.01
```






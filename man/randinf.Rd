% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_placebos.R
\name{randinf}
\alias{randinf}
\title{Randomization Inference}
\usage{
randinf(
  depvar,
  points.sf,
  lambdapoisl,
  nruns,
  id = "id",
  geometry = TRUE,
  sentinel_tries = 10,
  ...
)
}
\arguments{
\item{depvar}{the dependent variable as a string}

\item{points.sf}{sf data.frame containing all points and the relevant variables}

\item{lambdapoisl}{the parameter for the poisson line process (see \code{\link[spatstat]{rpoisline}} from \code{spatstat} for details)}

\item{nruns}{number of randomization runs (ideally in the thousands, start by trying a few dozen to check performance and speed)}

\item{id}{the unique id column in the points frame}

\item{geometry}{should the return results frame contain geometries so that all placebo/randomization lines can be plotted on a map?}

\item{sentinel_tries}{sometimes the poisson process creates lines that do not cross any points, thus leading to no control units (expected to happen by chance). This parameter determines how often the function should retry in case this happnes (default is 10).}

\item{...}{pass arguments to \code{rdrobust}}
}
\value{
a randomization inference p-value or, alternatively, a data frame containing all simulated lines with the respective estimates
}
\description{
Text
}
\examples{
points_samp.sf <- sf::st_sample(polygon_full, 1000) # create points
# make it an sf object bc st_sample only created the geometry list-column (sfc):
points_samp.sf <- sf::st_sf(points_samp.sf)
# add a unique ID to each observation:
points_samp.sf$id <- 1:nrow(points_samp.sf)
# assign treatment:
points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
# first we define a variable for the number of "treated" and control
NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
# the treated areas get a 10 percentage point higher literacy rate
points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
# and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 1]
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 0]

results.ri <- randinf("education", points_samp.sf, 0.0001, 100)

}
\references{
Baddeley A, Turner R (2005). “spatstat: An R Package for Analyzing Spatial Point Patterns.” Journal of Statistical Software, 12(6), 1–42. doi:10.18637/jss.v012.i06.
Baddeley A, Rubak E, Turner R (2015). Spatial Point Patterns: Methodology and Applications with R. Chapman and Hall/CRC Press, London. ISBN 9781482210200
}

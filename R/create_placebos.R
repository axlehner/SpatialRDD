


#' Multiple placebocheks unified in just one list or coefplot
#'
#' Unifies \code{\link{shift_border}}, \code{\link{cutoff2polygon}}, \code{\link{assign_treated}} in one function to carry out a myriad of placebo checks at once.
#' The output is either a data.frame (with or without geometry of the respective placeboline) or a coefplot.
#' Requires operations data.frame that contains all desired operations (columns shift.x, shift.y, scale, angle, orientation.1, orientation.2, endpoint.1, endpoint.2),
#' if you don't need a certain operation just use default values (e.g. 0 for angle and 1 for scale), but the column has to be there.
#'
#' @param data sf data.frame that contains all units of observation
#' @param cutoff initial RD cutoff as an sj object
#' @param formula provide the formula you want to use for OLS, omit the treatetment dummy (if you want a univariate regression just on "treated", then provide y ~ 1 as formula)
#' @param operations container that has all the information in it on how to change the border for each placeboregression
#' @param coefplot provide coefplot instead of a data.frame
#' @param geometry set to \code{TRUE} if you want to plot all the lines of the used placebo borders
#' @param bw_dist what is the distance for the bandwith (in CRS units, thus ideally metres)
#'
#' @return either a coefplot or data.frame containing results of placebo regressions
#' @export
#'
#' @examples
#' points_samp.sf <- sf::st_sample(polygon_full, 100) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' # add a unique ID to each observation:
#' points_samp.sf$id <- 1:nrow(points_samp.sf)
#' points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
#' operations.df <- data.frame(operation = c("shift"),
#'                             shift.x = c(0),
#'                             shift.y = c(0),
#'                             scale = 1,
#'                             angle = 0,
#'                             orientation.1 = c("west"),
#'                             orientation.2 = c("west"),
#'                             endpoint.1 = c(.8),
#'                             endpoint.2 = c(.2))
#' create_placebos(data = points_samp.sf, cutoff = cut_off,
#' formula = id ~ 1, operations = operations.df, bw_dist = 3000)

create_placebos <- function(data, cutoff, formula, operations, bw_dist,
                             coefplot = FALSE, geometry = FALSE
                             ) {

  # TODO
  # - so far it cannot accomodate the initial estimate with the initial treatment polygon
  # - on which side is the treated - have to define that in the absence of a treatment polygon

  # CRS CHECKS FIRST
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "cutoff not an sf object" = inherits(cutoff, "sf"),
    "CRS not matching between objects, transform them accordingly!"
    = sf::st_crs(data) == sf::st_crs(cutoff),

    "formula not provided correctly" = rlang::is_formula(formula)
  )

  #bw_dist <- 3000 # make this a parameter later
  # create the container -------------------------
  nruns <- nrow(operations)
  columnames <- c("id", "estimate", "std.error", "statistic", "p.value", "rd.estimate") # should we extract Ntr + Nco?
  results <- data.frame(matrix(ncol = length(columnames), nrow = nruns))
  colnames(results) <- columnames
  results$id <- 1:nruns

  # add multiple standard errors ----------------
  SE_types <- c("const", "HC0", "HC1", "HC2", "HC3", "HC4")
  SEs <- data.frame(matrix(ncol = length(SE_types), nrow = nruns))
  colnames(SEs) <- SE_types
  results <- cbind(results, SEs)

  # more SEs:
  # - cluster (come in w formula)
  # - Conley

  # geometry comes last:
  if (geometry == TRUE) results$geometry <- rep(NA, nrow(results))

  # create formula from input ------------
  # could equally well say: provide formula and then automatically update in the treated.1 in first posi
  formula <- stats::update(formula, ~ treated + .) # treated is always put on second position
  # KEY for all the following is that treated is on the 1st postion in the formula and thus 2nd coefficient in all the outputs (after the constant)
  y.string <- stats::formula(formula)[[2]]

    # loop over the placeboregressions -------------------------
  # ... later: option to parallelise! foreach or just make this loop one function and run it on parLapply or the purrr equiv? (since we have many params)
  for (i in 1:nrow(operations)) {
    #print(operations$angle[i])
    cutoff.1      <- shift_border(cutoff, operation = c("shift", "rotate", "scale"),
                                  shift = c(operations$shift.x[i], operations$shift.y[i]),
                                  scale = operations$scale[i],
                                  angle = operations$angle[i])
    polygon.1     <- try(cutoff2polygon(data = data, cutoff = cutoff.1,
                                    orientation = c(operations$orientation.1[i], operations$orientation.2[i]),
                                    endpoints   = c(operations$endpoint.1[i], operations$endpoint.2[i])))

    polygon.1     <- sf::st_make_valid(polygon.1) # redundancy bc cutoff2polygon also does taht
    # if the polygon comes out invalid we jump the loop, this is a safe fallback.
    if (sf::st_is_valid(polygon.1) == FALSE) {message("invalid treated polygon, jumping to next iteration! \n"); next}

    err <- FALSE # here we introduce some errorhandling
    #print(err)
    print(cutoff.1)
    data$treated <- tryCatch({assign_treated(data = data, polygon = polygon.1, id = "id")}, error = function(e) {err <<- TRUE})
    if (err) {message("err, next loop!"); next}
    data$dist2cutoff.1 <- as.numeric(sf::st_distance(data, cutoff.1)) %>% try() # compute distance to new border
    print("HERE")
    # NOW FOR THE NON-PARAMETRIC
    data$distrunning <- data$dist2cutoff.1
    # give the non-treated one's a negative score BECAUSE rdrobust recognizes positive score as treated
    data$distrunning[data$treated == 0] <- -1 * data$distrunning[data$treated == 0]
    # second regressor extracted (the treated):
    tryCatch({lm.obj <- stats::lm(formula, data = data[data$dist2cutoff.1 < bw_dist, ])
              results[i, 2:5] <- broom::tidy(lm.obj)[2, 2:5]
              results[i, 6:(5+length(SE_types))] <- sapply(SE_types, function(x) lmtest::coeftest(lm.obj, vcov = sandwich::vcovHC, type = x)[2, "t value"]) %>% t()
              #print(results[i, 6:(5+length(SE_types))])
              rd.obj <- rdrobust::rdrobust(data[[y.string]], data$distrunning, c = 0, p = 1)
              results$rd.estimate[i] <- rd.obj$Estimate[[1]]
              }, error = function(e) {err <<- TRUE})
    #print(err)
    if (err) {message("err, next loop!"); next}
    if (geometry == TRUE) results$geometry[i] <- sf::st_geometry(cutoff.1) %>% try()
  }
  # make it an sf object if we want to plot the lines on a map
  if (geometry == TRUE) results <- sf::st_sf(results, crs = sf::st_crs(data)) # this finds the geom column automatically

  results <- cbind(results, operations)

  if (coefplot == TRUE) {
    results %>% ggplot2::ggplot(mapping = ggplot2::aes(x = .data$id, y = .data$estimate,
                                                       ymin = (.data$estimate - 1.96 * .data$std.error),
                                                       ymax = (.data$estimate + 1.96 * .data$std.error))) +
      ggplot2::geom_errorbar(color = "grey") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      ggplot2::geom_point(ggplot2::aes(colour = cut(.data$p.value, c(-Inf, .11, Inf))), size = 1, shape = 19) +
      ggplot2::scale_color_manual(values = c("palegreen2", "lightcoral")) +
      # Here comes the styling
      ggplot2::theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
      ggplot2::theme(text = ggplot2::element_text(family = "Courier"), plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
      ggplot2::ggtitle(paste("Coefplot")) +
      ggplot2::labs(y = expression(beta~(treated)), x = "boundaryshift")

  } else results # if no coefplot requested we just return the data.frame with results

}

# replaced all T/F with TRUE/FALSE
# some error messages are remaining - they are needed


#' Randomization Inference
#'
#' Text
#'
#' @param depvar the dependent variable as a string
#' @param points.sf sf data.frame containing all points and the relevant variables
#' @param lambdapoisl the parameter for the poisson line process (see \code{\link[spatstat]{rpoisline}} from \code{spatstat} for details)
#' @param nruns number of randomization runs (ideally in the thousands, start by trying a few dozen to check performance and speed)
#' @param id the unique id column in the points frame
#' @param geometry should the return results frame contain geometries so that all placebo/randomization lines can be plotted on a map?
#' @param sentinel_tries sometimes the poisson process creates lines that do not cross any points, thus leading to no control units (expected to happen by chance). This parameter determines how often the function should retry in case this happnes (default is 10).
#'
#' @return a randomization inference p-value or, alternatively, a data frame containing all simulated lines with the respective estimates
#' @export
#'
#' @examples
#' points_samp.sf <- sf::st_sample(polygon_full, 1000) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' # add a unique ID to each observation:
#' points_samp.sf$id <- 1:nrow(points_samp.sf)
#' # assign treatment:
#' points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
#' # first we define a variable for the number of "treated" and control
#' NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
#' NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
#' # the treated areas get a 10 percentage point higher literacy rate
#' points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
#' points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
#' # and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
#' points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
#'   points_samp.sf$education[points_samp.sf$treated == 1]
#' points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
#'   points_samp.sf$education[points_samp.sf$treated == 0]
#'
#' results.ri <- randinf("education", points_samp.sf, 0.0001, 100)
#'
#' @references
#' Baddeley A, Turner R (2005). “spatstat: An R Package for Analyzing Spatial Point Patterns.” Journal of Statistical Software, 12(6), 1–42. doi:10.18637/jss.v012.i06.
#' Baddeley A, Rubak E, Turner R (2015). Spatial Point Patterns: Methodology and Applications with R. Chapman and Hall/CRC Press, London. ISBN 9781482210200

randinf <- function(depvar, points.sf,
                    lambdapoisl, nruns,
                    id = "id",
                    geometry = TRUE,
                    sentinel_tries = 10) {

  # create bbox from point input
  W <- spatstat.geom::as.owin(sf::st_as_sfc(sf::st_bbox(points.sf)))

  results.ri <- data.frame(run = 1:nruns) # create results table based on the number of desired runs
  results.ri$tconv <- NA; results.ri$tbiascorr <- NA; results.ri$trobust <- NA
  results.ri$point <- NA; results.ri$geometry <- NA

  for (i in 1:nruns) {


    # sentinel loop / value to re-try if the random line was out of bounds and did not result in T or C points
    # ... expected to happen, could also solve it by just printing an empty line (problem: user gets less runs than desired and it's hard to control the number)
    sent <- 1 # sentinel counter needs to be reset once we break out of the sentinel loop
    for (sent in 1:sentinel_tries) {
      err <- FALSE # reset error flag (if you loop inside here, you had an error, otherwise you break out)
      #cat("sentinel try:", sent)
      lines <- spatstat.random::rpoisline(lambda = lambdapoisl, win = W) # poisson line process
      lines.sf <- sf::st_as_sf(lines[1]) # pick the first line for every run (arbitrary!)
      sf::st_crs(lines.sf) <- sf::st_crs(points.sf)
      lines.sf <- lines.sf[2, ] # drop the window from the psp object

      # doing this here preserves the random lines that could not be used for placebo estimation
      if (geometry == TRUE) results.ri$geometry[i] <- sf::st_geometry(lines.sf) %>% try()

      # lwgeom, split the bbox with the random line to create two polygons
      splitted <- lwgeom::st_split(sf::st_as_sfc(sf::st_bbox(points.sf)), lines.sf$geom[1]) |>
        sf::st_collection_extract("POLYGON")

      # carry out the full Spatial RDD battery:
      points.sf$treated <- assign_treated(points.sf, sf::st_as_sf(splitted[1]), id = id) |> suppressWarnings()
      points.sf$distrunning <- sf::st_distance(points.sf, lines.sf$geom[1])
      points.sf$distrunning[points.sf$treated == 0] <- -1 * points.sf$distrunning[points.sf$treated == 0]

      # Estimation (need more flexibility later to allow for other specifications and an lm version)
      rdobj <- tryCatch({rdrobust::rdrobust(points.sf[[depvar]], points.sf$distrunning, c = 0, p = 1) |> suppressWarnings()}, error = function(e) {err <<- TRUE})
      if (err) {#message("err, next loop!");
        # notification if max sentinel numbers reached
        if (sent == sentinel_tries) message("could not construct a valid randomization exercise in one run, skipping... \n")
        next}
      results.ri$point[i] <- rdobj$coef[[1]]
      results.ri[i, 2:4]  <- rdobj$se[1:3] # paste all t-stats at once
      results.ri$pv[i]    <- rdobj$pv[[1]]
      if (!err) break # if there was no error, we simply break out of the sentinel loop and continue with our nrun loop

    }


  }
  # now just make it an sf data frame with the CRS of the project
  if (geometry == TRUE) results.ri <- sf::st_sf(results.ri, crs = sf::st_crs(lines.sf)) # this finds the geom column automatically

}


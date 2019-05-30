#' Spatial RD / GRD
#'
#' This is essentially the main function
#' It loops over all boundary points and locally estimates a non-parametric RD (local linear regression as usual)
#' using the rdrobust function from the rdrobust package from Calonico, Cattaneo, Titiunik (2014) Econometrica.x
#' It takes in the discretised cut-off point file (the RDcut-off linestring chopped into parts by the \code{\link{discretise_border}} function)
#' and the sf object (which essentially is just a conventional data.frame with a geometry() column) containing all the observations (treated and untreated)
#' the treated dummy variable has to be assigned before (potentially with \code{\link{assign_treated}}) and be part of the sf object as a column.
#'
#'
#' “An object is data with functions. A closure is a function with data.” — John D. Cook
#'
#' @param y The name of the dependent variable in the points frame in the form of a string
#' @param data sf object with points that describe the observations
#' @param cutoff.points the borderpoints that have been discretised before
#' @param treated column that contains the treated dummy (string)
#' @param minobs the minimum amount of observations in each estimation for the point estimate to be included
#' @param bwfix T/F if there should be a fixed bandwith applied to each single RD estimation
#' @param bwfix_m fixed bandwidth in meters
#' @param sample draw a random sample of points T/F
#' @param samplesize if random, how many points
#' @param cluster level at which standard errors should be clustered
#' @param spatial.object return a spatial object?
#'
#' @return a data.frame or spatial data.frame (sf object) in case spatial.object = T (default)
#' @export
#' @examples


SpatialRD <- function(y = "regressand", # should we make that a formula, in case we want to allow for the covariate stuff as well?
                       data = points.sf, cutoff.points = borderpoints.sf, treated = "treated", # data in, plus specify the name of the column where the dummy is AS STRING
                       minobs = 50, bwfix = F, bwfix_m = NA, # here we define parameters, minobs that have to be selected, bwfix in metres
                       sample = F, samplesize = NA,
                       cluster = NA, # string required
                       spatial.object = T # if this is 1, function will return an sf object
                       ) {

  # TODO
  # - fix the issue at the dist2cutoff st_crs dimension error with st_distance when different types of borderpoints come into play
  # - this plays also in the np determination (length vs nrow in the KTcase)
  # - solution to exclude sparse borderpoints before rdrobust(), check within buffer of c. 10km, if too little, jump the loop iteration

  # no deparse(substitute(colname)), require the string directly

  # message 1

  n <- nrow(data)
  cat("We have", n, "observations of which", sum(as.numeric(as.character(data[[treated]]))), "are treated observations.\n")

  if (!is.na(bwfix_m)) {bwfix <- T}

  # bordersamplesize
  if (sample == T) {
    if(is.na(samplesize)) {cat("Random border point sampling chosen: provide desired samplesize!\n")}
    border_id <- sample(1:nrow(cutoff.points), samplesize)
    # sort it so we loop beginning from the first one
    border_id <- border_id[order(border_id)]
    cutoff.points <- cutoff.points[border_id, ]
    np <- length(cutoff.points)
    cat("Choosing", np, "random points on the border to iterate over.\n")
  } else {
    np <- length(cutoff.points) #replaced the nrow with length. why? need nrow. NO only for points., hae?
    cat("We are iterating over", np, "Boundarypoints.\n")
  }

  cat("The dependent variable is", y,".\n")
  # create storage for final results
  #columnames <- c("Point", "Estimate", "pvalC", "pvalR", "Ntr", "Nco", "bw", "CI_Conv", "CI_Rob", "McCrary", "RATest")
  # the version where the CIs have upper and lower in a separate column
  columnames <- c("Point", "Estimate", "pvalC", "pvalR", "Ntr", "Nco", "bw", "CI_Conv_l", "CI_Conv_u", "CI_Rob_l", "CI_Rob_u", "McCrary", "RATest")
  results <- data.frame(matrix(ncol = length(columnames), nrow = np))
  colnames(results) <- columnames

  # creating the column for the distance, gets overwritten in every loop
  data[["dist2cutoff"]] <- 0

  for (i in 1:np) {

    # preparation
    #----------------------
    # create distance to boundarypoint (using sf package and making the vector numeric, drop units [m])

    if (bwfix == T) {bw <- bwfix_m}

    # this would be the dplyr vector way, NON-CLUMSY
    # as.numeric(st_distance(data, cutoff.points[i, ], drop = T)) %>% .[. < bwfix_m]
    data[["dist2cutoff"]] <- as.numeric(sf::st_distance(data, cutoff.points[i], drop = T)) # rewrote the subsetting from [i, ]

    # this is after the KT replication. need to choose min
    # May19, this doesn't work with the simulated data. different boundarypoint format. have to convert the KT boundarypoints?
    #data[["dist2cutoff"]] <- as.numeric(sf::st_distance(data, cutoff.points[i, ], drop = T)) # rewrote the subsetting to [i, ]


    # weight (redundant)
    #w <- as.vector((1 / bw) * Kt((data[["dist2cutoff"]] - 0) / bw))
    #sum(w > 0)
    # this the number of positive weights

    # make distance of treated data negative (here it was rewritten that the untreated are negative so that the sign of the coefficient is right)
    data[["dist2cutoff"]][data[[treated]] == 0] <- data[["dist2cutoff"]][data[[treated]] == 0] * (-1)
    data[["dist2cutoff"]] <- data[["dist2cutoff"]] #/ 1000 #make it km
    #bw <- bw / 1000

    # about here i need the check whether we have enough observations around the point for the estimation
    # what defines a sparse borderpoint?

    # estimation
    #----------------------
    # if else with BW being the first decision
    if (bwfix == T) {

      if (is.na(cluster)) { # if nothing was specified, the default is without clustering:
        rdrob_bwflex <- rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, h = bw)#, covs = cbind(data[["Lat"]], data[["Long"]]))
      } else { # this runs when a cluster was specified as a string
        rdrob_bwflex <- rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, h = bw, cluster = data[[cluster]])
      }
    } else { # this is the same, just with MSE-optimal bandwidth

      if (is.na(cluster)) { # if nothing was specified, the default is without clustering:
        rdrob_bwflex <- try(rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0))#, covs = cbind(data[["Lat"]], data[["Long"]]))
      } else { # this runs when a cluster was specified as a string
        rdrob_bwflex <- try(rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, cluster = data[[cluster]]))
      }

    }

    # RD testing
    #invisible(capture.output(mccrary <- dc_test(data[["dist2cutoff"]]), plot = F))
    # switch to another mccrary, probably from the rdd itself, or the RDDtools?
    invisible(capture.output(permtest <- RATest::RDperm(W = c(y), z = "dist2cutoff", data = as.data.frame(data))))

    # store results
    #========================
    results[i, "Point"] <- i
    results[i, "Estimate"] <- round(rdrob_bwflex[["coef"]][["Conventional", 1]], 2)
    results[i, "pvalC"] <- round(rdrob_bwflex[["pv"]][["Conventional", 1]], 2)
    #results[i, "pvalB-C"] <- round(rdrob_bwflex[["pv"]][["Bias-Corrected", 1]], 2)
    results[i, "pvalR"] <- round(rdrob_bwflex[["pv"]][["Robust", 1]], 2)
    results[i, "Ntr"] <- rdrob_bwflex[["Nh"]][[1]]
    results[i, "Nco"] <- rdrob_bwflex[["Nh"]][[2]]
    results[i, "bw"] <- round(rdrob_bwflex[["bws"]][["h", 1]] / 1000, 1)
    #results[i, "CI_Conv"] <- paste0("[",round(rdrob_bwflex$ci["Conventional", 1], 2), ";", round(rdrob_bwflex$ci["Conventional", 2], 2), "]", sep = "")
    #results[i, "CI_Rob"] <- paste0("[",round(rdrob_bwflex$ci["Robust", 1], 2), ";", round(rdrob_bwflex$ci["Robust", 2], 2), "]", sep = "")
    # this is for the version with lower upper CIs separated:
    results[i, "CI_Conv_l"] <- round(rdrob_bwflex$ci["Conventional", 1], 2)
    results[i, "CI_Conv_u"] <- round(rdrob_bwflex$ci["Conventional", 2], 2)
    results[i, "CI_Rob_l"] <- round(rdrob_bwflex$ci["Robust", 1], 2)
    results[i, "CI_Rob_u"] <- round(rdrob_bwflex$ci["Robust", 2], 2)

    #results[i, "McCrary"] <- round(mccrary, 3)
    results[i, "RATest"] <- round(permtest$results[2], 3)
  }
  # set the geometry of the borderpoints before subsetting according to the minobs criterion
  if (spatial.object == T) {results <- sf::st_set_geometry(results, sf::st_geometry(cutoff.points))}
  # this selects ultimately only the boundarypoint estimates that have a large enough sample size on both sides in order to ensure proper inference
  results %>% dplyr::filter(Nco > minobs & Ntr > minobs)

  # MAKE THIS RESULTS LIST A LIST
  # AND THEN PRODUCE ALSO STANDARD OUPUT! that is printed when u run it

}

#'
#' This is essentially the main function
#' It loops over all boundary points and locally estimates a non-parametric RD (local linear regression as usual)
#' using the rdrobust function from the rdrobust package from Calonico, Cattaneo, Titiunik (2014) Econometrica
#'
#' It takes in the discretised cut-off point file (the RDcut-off linestring chopped into parts by the SpatialRDD::discretise_border() function)
#' and the sf object (which essentially is just a conventional data.frame with a geometry() column) containing all the observations (treated and untreated)
#' the treated dummy variable has to be assigned before and be part of the sf object as a column
#' distances etc are all computed within the function using sf
#'
#' “An object is data with functions. A closure is a function with data.” — John D. Cook
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples


SpatialRD <- function(y = "regressand", # should we make that a formula, in case we want to allow for the covariate stuff as well?
                       data = points.sf, cutoff.points = borderpoints.sf, treated = "treated", # data in, plus specify the name of the column where the dummy is AS STRING
                       minobs = 50, bwfix = F, bwfix_m = NA, # here we define parameters, minobs that have to be selected, bwfix in metres
                       sample = F, samplesize = 10,
                       cluster = NA # string required
                       ) {

  # no deparse(substitute(colname)), require the string directly

  # message 1

  n <- nrow(data)
  cat("We have", n, "observations of which", sum(as.numeric(data[["treated"]])), "are treated observations.\n")

  if (!is.na(bwfix_m)) {bwfix <- T}

  # bordersamplesize
  if (sample == T) {
    border_id <- sample(1:nrow(cutoff.points), samplesize)
    # sort it so we loop beginning from the first one
    border_id <- border_id[order(border_id)]
    cutoff.points <- cutoff.points[border_id, ]
    np <- nrow(cutoff.points)
    cat("Choosing", np, "random points on the border to iterate over.\n")
  } else {
    np <- length(cutoff.points) #replaced the nrow with length
    cat("We are iterating over", np, "Boundarypoints.\n")
  }

  cat("The dependent variable is", y,".\n")
  # create storage for final results
  columnames <- c("Point", "Estimate", "pvalC", "pvalR", "Ntr", "Nco", "bw", "CI_Conv", "CI_Rob", "McCrary", "RATest")
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
    data[["dist2cutoff"]] <- as.numeric(st_distance(data, cutoff.points[i], drop = T)) # rewrote the subsetting from [i, ]

    # weight (redundant)
    #w <- as.vector((1 / bw) * Kt((data[["dist2cutoff"]] - 0) / bw))
    #sum(w > 0)
    # this the number of positive weights

    # make distance of treated data negative
    data[["dist2cutoff"]][data[["treated"]] == 1] <- data[["dist2cutoff"]][data[["treated"]] == 1] * (-1)
    data[["dist2cutoff"]] <- data[["dist2cutoff"]] #/ 1000 #make it km
    #bw <- bw / 1000

    # estimation
    #----------------------
    # if else with BW being the first decision
    if (bwfix == T) {

      if (is.na(cluster)) { # if nothing was specified, the default is without clustering:
        rdrob_bwflex <- rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, h = bw)#, covs = cbind(data[["Lat"]], data[["Long"]]))
      } else { # this runs when a cluster was specified as a string
        rdrob_bwflex <- rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, h = bw, cluster = data[[cluster]])
      }
    } else { # this is the same, just with MSE-optimal bandwidth

      if (is.na(cluster)) { # if nothing was specified, the default is without clustering:
        rdrob_bwflex <- rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0)#, covs = cbind(data[["Lat"]], data[["Long"]]))
      } else { # this runs when a cluster was specified as a string
        rdrob_bwflex <- rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, cluster = data[[cluster]])
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
    results[i, "CI_Conv"] <- paste0("[",round(rdrob_bwflex$ci["Conventional", 1], 2), ";", round(rdrob_bwflex$ci["Conventional", 2], 2), "]", sep = "")
    results[i, "CI_Rob"] <- paste0("[",round(rdrob_bwflex$ci["Robust", 1], 2), ";", round(rdrob_bwflex$ci["Robust", 2], 2), "]", sep = "")
    #results[i, "McCrary"] <- round(mccrary, 3)
    results[i, "RATest"] <- round(permtest$results[2], 3)
  }

  # this selects ultimately only the boundarypoint estimates that have a large enough sample size on both sides in order to ensure proper inference
  results %>% dplyr::filter(Nco > minobs & Ntr > minobs)


}

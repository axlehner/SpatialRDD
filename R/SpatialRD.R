#' non-parametric Spatial RD / GRD
#'
#'
#' This function loops over all boundary points and locally estimates a non-parametric RD (local linear regression as usual)
#' using the rdrobust function from the rdrobust package from Calonico, Cattaneo, Titiunik (2014) Econometrica.
#' It takes in the discretised cut-off point file (the RDcut-off linestring chopped into parts by the \code{\link{discretise_border}} function)
#' and the sf object (which essentially is just a conventional data.frame with a geometry() column) containing all the observations (treated and untreated).
#' The treated dummy variable has to be assigned before (potentially with \code{\link{assign_treated}}) and be part of the sf object as a column.
#'
#' This function nests \code{\link[rdrobust]{rdrobust}}. All its options (aside from running variable \code{x} and cutoff \code{c}) are available here as well (e.g. cluster level, kernel, weights).
#' Check the documentation in the \code{rdrobust} package for details.
#'
#' To visualise the output table, use \code{\link{printspatialrd}} or \code{\link{plotspatialrd}}.
#'
#'
#' @param y The name of the dependent variable in the points frame in the form of a string
#' @param data sf data.frame with points that describe the observations
#' @param cutoff.points sf object of borderpoints (provided by user or obtained with \code{\link{discretise_border}})
#' @param treated column that contains the treated dummy (as string)
#' @param minobs the minimum amount of observations in each estimation for the point estimate to be included (default is 50)
#' @param bwfix_m fixed bandwidth in meters
#' @param sample draw a random sample of points T/F
#' @param samplesize if random, how many points
#' @param spatial.object return a spatial object (needed if you want to plot the point estimates on a map)?
#' @param RATestvec vector with strings of basline covariates in the data object in case the RATest by Canay, Kamat (2018) is desired
#' @param sparse.exclusion in case we want to try to exclude sparse border points before the estimation (should reduce warnings)
#' @param ... in addition you can use all options in \code{\link[rdrobust]{rdrobust}}
#'
#' @return a data.frame or spatial data.frame (sf object) in case spatial.object = T (default)
#' @export
#'
#' @examples \dontrun{results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf,
#' treated = "treated", minobs = 10, spatial.object = F)}


spatialrd <- function(y,
                       data, cutoff.points, treated, # data in, plus specify the name of the column where the dummy is AS STRING
                       minobs = 50, bwfix_m = NA, # here we define parameters, minobs that have to be selected, bwfix in metres
                       sample = F, samplesize = NA,
                       sparse.exclusion = F,
                       #cluster = NA, # string required
                       spatial.object = T, # if this is 1, function will return an sf object
                       RATestvec = NA, ...
                       ) {

  # TODO
  # - fix the issue at the dist2cutoff st_crs dimension error with st_distance when different types of borderpoints come into play
  # - this plays also in the np determination (length vs nrow in the KTcase)
  # - solution to exclude sparse borderpoints before rdrobust(), check within buffer of c. 10km, if too little, jump the loop iteration
  # --- LIKE KT16 propose!
  # - incorporate the swagger (separate function or righthere?)
  # - color for pvalue intensity?
  # - option to kick pvalue from results table

  # no deparse(substitute(colname)), require the string directly

  # CHECKS ON SF OBJECTS
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "treatment polygon is not an sf object" = inherits(cutoff.points, "sf"),
    "CRS not matching between objects, transform them accordingly!"
    = sf::st_crs(data)$input == sf::st_crs(cutoff.points)$input,

    "treated column not specified correctly. Is it a string?" = inherits(treated, "character"),
    "dependent variable not specified correctly. Is it a string?" = inherits(y, "character")
  )

  # message 1

  n <- nrow(data)
  cat("We have", n, "observations of which", sum(as.numeric(as.character(data[[treated]]))), "are treated observations.\n")


  bwfix <- F
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
    np <- nrow(cutoff.points) #replaced the nrow with length. why? need nrow. NO only for points., hae?
    cat("We are iterating over", np, "Boundarypoints.\n")
  }


  #######################################
  # this was the initial way of excluding points, but the more direct way is
  #  to just run the estimation and then exclude afterwards (even though it creates annoying warnings)
  # excluding this once caused during R CMD check: Error in process_get_error_connection(self, private) : stderr is not a pipe.
  if (sparse.exclusion == T) {
    # Triangular or Edge kernel function
    Kt = function(u) {
      (1-abs(u)) * (abs(u) <= 1)
    }
  }
  ########################################
  # after this we also define the number of points newly

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
    data[["dist2cutoff"]] <- as.numeric(sf::st_distance(data, cutoff.points[i,], drop = T)) # rewrote the subsetting from [i, ]

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
      rdrob_bwflex <- rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, h = bw, ...)
    } else { # this is the same, just with MSE-optimal bandwidth
      rdrob_bwflex <- try(rdrobust::rdrobust(data[[y]], x = data[["dist2cutoff"]], c = 0, ...))
    }

    # RD testing
    #invisible(utils::capture.output(mccrary <- dc_test(data[["dist2cutoff"]]), plot = F))
    # switch to another mccrary, probably from the rdd itself, or the RDDtools?
    # is based on the DCdensity from rdd
    mccrary <- rdd::DCdensity(data[["dist2cutoff"]], 0, plot = F)
    # mccrary <- rddapp::dc_test(data[["dist2cutoff"]], verbose = F, plot = F, ext.out = F), same results, bad output, the rdd:: is the original one in any case

    if (!is.na(RATestvec)) {
      invisible(utils::capture.output(permtest <- RATest::RDperm(W = RATestvec, z = "dist2cutoff", data = as.data.frame(data))))
    }
    # store results
    #========================
    results[i, "Point"] <- i
    results[i, "Estimate"] <- round(rdrob_bwflex[["coef"]][["Conventional", 1]], 2)
    results[i, "pvalC"] <- round(rdrob_bwflex[["pv"]][["Conventional", 1]], 2)
    #results[i, "pvalB-C"] <- round(rdrob_bwflex[["pv"]][["Bias-Corrected", 1]], 2)
    results[i, "pvalR"] <- round(rdrob_bwflex[["pv"]][["Robust", 1]], 2)
    # N_h is eff sample size, what is N_b?
    results[i, "Ntr"] <- rdrob_bwflex[["N_h"]][[1]]
    results[i, "Nco"] <- rdrob_bwflex[["N_h"]][[2]]
    results[i, "bw"] <- round(rdrob_bwflex[["bws"]][["h", 1]] / 1000, 1)
    #results[i, "CI_Conv"] <- paste0("[",round(rdrob_bwflex$ci["Conventional", 1], 2), ";", round(rdrob_bwflex$ci["Conventional", 2], 2), "]", sep = "")
    #results[i, "CI_Rob"] <- paste0("[",round(rdrob_bwflex$ci["Robust", 1], 2), ";", round(rdrob_bwflex$ci["Robust", 2], 2), "]", sep = "")
    # this is for the version with lower upper CIs separated:
    results[i, "CI_Conv_l"] <- round(rdrob_bwflex$ci["Conventional", 1], 2)
    results[i, "CI_Conv_u"] <- round(rdrob_bwflex$ci["Conventional", 2], 2)
    results[i, "CI_Rob_l"] <- round(rdrob_bwflex$ci["Robust", 1], 2)
    results[i, "CI_Rob_u"] <- round(rdrob_bwflex$ci["Robust", 2], 2)

    results[i, "McCrary"] <- round(mccrary, 2)
    if (!is.na(RATestvec)) {results[i, "RATest"] <- round(permtest$results[2], 2)}
  }
  # subset away what we don't want (can put if condition as function argument lateron)
  # results <- results %>% select(-c(pvalC, pvalR)) # no p-values, but for now we have to keep them otherwise cannot make the plots

  results[["Ntr"]] <- as.integer(results[["Ntr"]]) # make them integers so the results table doesn't get printed with .0
  results[["Nco"]] <- as.integer(results[["Nco"]])

  # this is bruteforce, but kick the McCr n RA this way, later we decide if we want them

  results <- results %>%
    dplyr::select(-c(.data$McCrary)) %>% # kick McCrary
    dplyr::select(-c(.data$RATest)) # kick RATest

  # set the geometry of the borderpoints before subsetting according to the minobs criterion
  if (spatial.object == T) {results <- sf::st_set_geometry(results, sf::st_geometry(cutoff.points))}
  # this selects ultimately only the boundarypoint estimates that have a large enough sample size on both sides in order to ensure proper inference
  results %>% dplyr::filter(.data$Nco > minobs & .data$Ntr > minobs)

  # MAKE THIS RESULTS LIST A LIST
  # AND THEN PRODUCE ALSO STANDARD OUPUT! that is printed when u run it

}




#' Print spatialrd output
#'
#' Preliminary function, styling with e.g. kable and kableExtra has to be done by the user individually.
#' You could also just use the package of your choice to print out columns of the output from \code{\link{spatialrd}}.
#'
#' @param SpatialRDoutput output file from the \code{\link{spatialrd}} function
#'
#' @return A table with results from the \code{\link{spatialrd}} function
#' @export
#'
#' @examples
#'
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
#' # create distance to cutoff
#' points_samp.sf$dist2cutoff <- as.numeric(sf::st_distance(points_samp.sf, cut_off))
#'
#' points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
#' # give the non-treated one's a negative score
#' points_samp.sf$distrunning[points_samp.sf$treated == 0] <- -1 *
#'  points_samp.sf$distrunning[points_samp.sf$treated == 0]
#'
#' # create borderpoints
#' borderpoints.sf <- discretise_border(cutoff = cut_off, n = 10)
#' borderpoints.sf$id <- 1:nrow(borderpoints.sf)
#'
#' # finally, carry out estimation alongside the boundary:
#' results <- spatialrd(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf,
#' treated = "treated", minobs = 20, spatial.object = FALSE)
#' printspatialrd(results)
#'
printspatialrd <- function(SpatialRDoutput#, #label = NA, caption = NA, footnote = NA,
                           #format = "latex"
                           ) {

  # TODO

  # - make pvalue optional
  # - make WATE optional

  SpatialRDoutput <- rbind(SpatialRDoutput,
                           SpatialRDoutput %>% dplyr::summarise(dplyr::across(dplyr::everything(), mean))
  )
  SpatialRDoutput[nrow(SpatialRDoutput), "Point"] <- "Mean"
  SpatialRDoutput
  #mutate or apply job, with this we kill the decimal places
  #SpatialRDoutput <- SpatialRDoutput %>% dplyr::mutate_at(dplyr::vars(.data$Ntr:.data$Nco), dplyr::funs(round(.data$., 0))) #this looks likea weird solution for the "." prob

  # omit all the kable styling, user has to do that themselves
  # sf::st_set_geometry(SpatialRDoutput, NULL) %>% # make the non-spatiality on the fly here so that I don't have to estimate it again for the mapplot
  # SpatialRDoutput %>% # if the object is non-spatial
  #   dplyr::select(-c(.data$p_Conv, .data$p_Rob)) %>% # kick the pvalues (need them for plot later)
  #   #dplyr::select(-c(.data$McCrary)) %>% # kick McCrary
  #   #dplyr::select(-c(.data$RATest)) %>% # kick RATest
  #   kableExtra::kable(label = .data$label, caption = .data$caption,
  #         digits = 2, row.names = FALSE, format = format, booktabs = TRUE, align = "c", longtable = TRUE) %>% # "latex"
  #   kableExtra::kable_styling(full_width = TRUE, latex_options = c("repeat_header")) %>% # "hold_position" removed temp, "repeat_header" prob not needed atm
  #   kableExtra::column_spec(1, width = ".5cm") %>% # point column
  #   kableExtra::column_spec(3:5, width = ".5cm") %>% #Ntr to bw columns
  #   kableExtra::column_spec(6:9, width = "1.4cm") %>% #the confidence intervals
  #   kableExtra::column_spec(10:11, width = ".9cm") %>% #McCrary and RAtest column
  #   kableExtra::row_spec(nrow(SpatialRDoutput)-1, hline_after = TRUE) %>% # then we go in with summary stats etc
  #   kableExtra::footnote(general = .data$footnote,
  #            #alphabet = "The rows below .", # how to deal with this multiple stuff
  #            threeparttable = TRUE)


}

# corrected T/F

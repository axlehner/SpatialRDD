


#' Print spatialrd output
#'
#' Preliminary function using kable and kableextra.
#' You could also just use the package of your choice to print out columns of the output from \code{\link{spatialrd}}.
#'
#' @param SpatialRDoutput output file from the \code{\link{spatialrd}} function
#' @param format for now only latex
#'
#' @return A formatted table with results from the \code{\link{spatialrd}} function
#' @export
#'
#' @examples \dontrun{printspatialrd(results.spatialrd)}
printspatialrd <- function(SpatialRDoutput, #label = NA, caption = NA, footnote = NA,
                           format = "latex"
                           #McCrary = F, RATest = F
                           ) {

  # TODO

  # - make pvalue optional
  # - make WATE optional

  SpatialRDoutput <- rbind(SpatialRDoutput,
                           SpatialRDoutput %>% dplyr::summarise_all(dplyr::funs(mean)))
  SpatialRDoutput[nrow(SpatialRDoutput), "Point"] <- "Mean"

  #mutate or apply job, with this we kill the decimal places
  SpatialRDoutput <- SpatialRDoutput %>% dplyr::mutate_at(dplyr::vars(.data$Ntr:.data$Nco), dplyr::funs(round(.data$., 0))) #this looks likea weird solution for the "." prob


  # sf::st_set_geometry(SpatialRDoutput, NULL) %>% # make the non-spatiality on the fly here so that I don't have to estimate it again for the mapplot
  SpatialRDoutput %>% # if the object is non-spatial
    dplyr::select(-c(.data$pvalC, .data$pvalR)) %>% # kick the pvalues (need them for plot later)
    dplyr::select(-c(.data$McCrary)) %>% # kick McCrary
    dplyr::select(-c(.data$RATest)) %>% # kick RATest
    kableExtra::kable(label = .data$label, caption = .data$caption,
          digits = 2, row.names = F, format = "latex", booktabs = T, align = "c", longtable = T) %>% # "latex"
    kableExtra::kable_styling(full_width = T, latex_options = c("repeat_header")) %>% # "hold_position" removed temp, "repeat_header" prob not needed atm
    kableExtra::column_spec(1, width = ".5cm") %>% # point column
    kableExtra::column_spec(3:5, width = ".5cm") %>% #Ntr to bw columns
    kableExtra::column_spec(6:9, width = "1.4cm") %>% #the confidence intervals
    kableExtra::column_spec(10:11, width = ".9cm") %>% #McCrary and RAtest column
    kableExtra::row_spec(nrow(SpatialRDoutput)-1, hline_after = T) %>% # then we go in with summary stats etc
    kableExtra::footnote(general = .data$footnote,
             #alphabet = "The rows below .", # how to deal with this multiple stuff
             threeparttable = T)


}

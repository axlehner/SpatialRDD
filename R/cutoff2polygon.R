
#' Create (treated) polygon from line
#'
#' Creates an approximation of a "treated/untreated polygon" to assign the status again to each observation after the border has been shifted.
#' The function extends both ends of the provided cutoff to the edge of the (imaginary) bounding box of the provided data (this ensures all observations will be included).
#' Key is that you provide a 2-tuple that indicates in which side of the bounding box each end should go (1st element is the one with lower x-coordinate, i.e. leftern most). Always check the output manually by plotting the polygon (e.g. with \code{tm_shape(your.polygon) + tm_polygons()}).
#' If the output polygon looks odd, a first check should be to just switch the elements from the orientation vector around! See \code{vignette(shifting_borders)} for details and illustrative examples.
#'
#' @param data study dataset to determine the bounding box (so that all observations are covered by the new polygons) in sf format
#' @param cutoff sf object of the (placebo) cut-off
#' @param orientation in which side of the bounding box does each of the extensions of the cutoff go into? First element refers to endpoint of border with smaller x-coordinate ("westernmost") (takes two of "north", "east", "south", "west" in a vector, e.g. \code{c("west", "north")})
#' @param endpoints at what position on the edge should each polygon end? (vector with two numbers between 0 and 1, where 0.5 e.g. means right in the middle of the respective edge)
#'
#' @return a polygon as an sf object
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' points_samp.sf <- sf::st_sample(polygon_full, 100) # create points
#' # make it an sf object bc st_sample only created the geometry list-column (sfc):
#' points_samp.sf <- sf::st_sf(points_samp.sf)
#' # add a unique ID to each observation:
#' points_samp.sf$id <- 1:nrow(points_samp.sf)
#' cutoff2polygon(data = points_samp.sf, cutoff = cut_off,
#' orientation = c("west", "west"), endpoints = c(.8, .2))


cutoff2polygon <- function(data, cutoff,
                            orientation = NA, endpoints = c(0, 0)) {
  # CHANGELOG
  # - hardcoded the corner(s) for each of the 16 possible combinations (4 for each side)

  # TODO
  # - give option to produce the logical negation (instead of asking user to use the assign_treated() for the untreated)

  # log to avoid a RMD CHECK problem:
  # global vars to avoid "no visible binding" error when doing R CMD check
  #utils::globalVariables(c("X"))
  # use .data$ (provided that you’ve also imported rlang::.data with @importFrom rlang .data )."

  # CHECKS ON SF OBJECTS
  stopifnot(
    "data frame is not an sf object"        = inherits(data, "sf"),
    "cutoff line is not an sf object"       = inherits(cutoff, "sf"),
    "CRS not matching between objects, transform them accordingly!"
    = sf::st_crs(data) == sf::st_crs(cutoff),

    "Orientation not specified correctly. Is it a string?" = inherits(orientation, "character"),
    "Provide two elements in the orientation vector. One for each side in which the extended boundary should go into." = length(orientation) == 2
  )

  crs <- sf::st_crs(cutoff)

  # HARDCODING START --------------------------- (clockwise logic already from here)
  # 1) NO CORNER PAIRS
  if (all(orientation == c("north", "north")) | all(orientation == c("North", "North"))) {corners <- 0; poly_c_start <- c(NA, NA)}
  if (all(orientation == c("east", "east"))   | all(orientation == c("East", "East"))) {corners <- 0; poly_c_start <- c(NA, NA)}
  if (all(orientation == c("south", "south")) | all(orientation == c("South", "South"))) {corners <- 0; poly_c_start <- c(NA, NA)}
  if (all(orientation == c("west", "west"))   | all(orientation == c("West", "West"))) {corners <- 0; poly_c_start <- c(NA, NA)}
  # 2) ONE CORNER CASES
  if (all(orientation == c("north", "east")) | all(orientation == c("North", "East"))) {corners <- 1; poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}
  if (all(orientation == c("east", "north")) | all(orientation == c("East", "North"))) {corners <- 1; poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}

  if (all(orientation == c("north", "west")) | all(orientation == c("North", "West"))) {corners <- 4; poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}
  if (all(orientation == c("west", "north")) | all(orientation == c("West", "North"))) {corners <- 4; poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}

  if (all(orientation == c("south", "east")) | all(orientation == c("South", "East"))) {corners <- 2; poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}
  if (all(orientation == c("east", "south")) | all(orientation == c("East", "South"))) {corners <- 2; poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}

  if (all(orientation == c("south", "west")) | all(orientation == c("South", "West"))) {corners <- 3; poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}
  if (all(orientation == c("west", "south")) | all(orientation == c("West", "South"))) {corners <- 3; poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}
  # 3) TWO CORNER CASES (straight across the bbox) - having a 2nd start poly now
  if (all(orientation == c("north", "south")) | all(orientation == c("North", "South"))) {corners <- c(3, 4); poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin']); poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}
  if (all(orientation == c("south", "north")) | all(orientation == c("South", "North"))) {corners <- c(1, 2); poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax']); poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}

  if (all(orientation == c("east", "west")) | all(orientation == c("East", "West"))) {corners <- c(4, 1); poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax']); poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}
  if (all(orientation == c("west", "east")) | all(orientation == c("West", "East"))) {corners <- c(2, 3); poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin']); poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}

  offsetX <- 0
  offsetY <- 0

  # OLD CORNERING GRAVEYARD (keep for future reference):
  # if (is.na(corners) | corners[1] == 0) {if (messages == T) message("\n No corners selected, thus both extensions will end in the same side.\n")
  #   corners <- c(0)
  #   poly_c_start <- c(NA, NA)
  #   }
  # # STARTING CORNER
  # if (corners[1] == 1) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}
  # if (corners[1] == 2) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}
  # if (corners[1] == 3) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}
  # if (corners[1] == 4) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}
  # # this is not used if no corner was given, above it was defaulted to a vector with a 0 then
  #
  # # IF 2nd CORNER SPECIFIED: (going clockwise)
  # # we don't give you the choice here, the first corner uniquely defines the 2nd
  # # so if 2nd element is non.NA we take the next corner point of the bounding box
  # if (corners[1] == 1 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])} # 2nd after 1st
  # if (corners[1] == 2 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])} # 3rd after 2nd
  # if (corners[1] == 3 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])} # 4th after 3rd
  # if (corners[1] == 4 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])} # 1st after 4th


  # HERE WE START THE UPPER PART (rbind the first point)
  if (orientation[1] == "North" | orientation[1] == "north") {
    poly_1_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[1], # % of the X-axis
                            sf::st_bbox(data)['ymax']) # stay on the northern bound
                          )


  } else if (orientation[1] == "East" | orientation[1] == "east") {
    poly_1_bound <- rbind(c(sf::st_bbox(data)['xmax'], # stay on the eastern bound
                            sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[1]) # % of the Y-axis
                          )


  } else if (orientation[1] == "South" | orientation[1] == "south") {
    poly_1_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[1], # % of the X-axis
                            sf::st_bbox(data)['ymin']) # stay on the southern bound
                          )

  } else if (orientation[1] == "West" | orientation[1] == "west") {

    poly_1_bound <- rbind(#c(sf::st_bbox(data)['xmin'], # stay on the west border
                          # (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[2] )), # Y-axis shifter
                          # start right at the prolongued point instead of bottom right
                          c(sf::st_bbox(data)['xmin'], # stay on the west border
                            sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[1]) # Y-axis shifter 4 upperpoint
                          )

  }

  # BIND IT TOGETHER WITH THE ACTUAL CUTOFF (make the first point a line together with the cutoff)
  # ... condition instead of is.na(corner[2])
  if (length(corners) == 1) {poly_1 <- poly_c_start %>% rbind(poly_1_bound)} else { # if there is no 2nd corner, we just bind with starting border
                          poly_1 <- rbind(poly_c_start, poly_c_2nd, poly_1_bound) # if 2nd corner specified, bind 2nd corner (clockwise) inbetween
  }

  colnames(poly_1) <- c("X", "Y")
  poly_1 <- poly_1 %>% rbind(as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)])# bind the upper part with the cutoff line


  # HERE WE CONTINUE WITH THE BOTTOM PART (bind the other point on the bbox doundary)
  if (orientation[2] == "North" | orientation[2] == "north") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[2], # % of the X-axis
                            sf::st_bbox(data)['ymax']) # stay on the northern bound
                         )


  } else if (orientation[2] == "East" | orientation[2] == "east") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmax'], # stay on the eastern bound
                            sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[2]) # % of the Y-axis
                          )



  } else if (orientation[2] == "South" | orientation[2] == "south") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[2], # % of the X-axis
                            sf::st_bbox(data)['ymin']) # stay on the southern bound
                         )


  } else if (orientation[2] == "West" | orientation[2] == "west") {
    # now we go down again to the lower prolongued point
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmin'], # stay on west border
                           (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[2])) # Y-axis shifter
                         )

  }
  # ENDING CORNER (going into the starting one to close the polygon)
  # not needed anymore as we just recycle the first element that was saved in order to close the polygon
  # if (corners[1] == 1) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}
  # if (corners[1] == 2) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}
  # if (corners[1] == 3) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}
  # if (corners[1] == 4) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}
  poly_2 <- rbind(poly_2_bound, poly_c_start)

  colnames(poly_2) <- c("X", "Y")
  if (corners[1] == 0) {poly_2 <- rbind(poly_2, poly_1_bound)} # if we have no corner we have to RE-TIE with the first point, as there is no natural starting point
  # closer has to be in last position, same value as first position

  # REMOVE NA'S RESULTING FROM NO CORNER CASES (even if no corner we take NA's in because cleaning them out here is easier than excluding them in the first place with)

  no_NAs <- as.data.frame(list(as.matrix(rbind(poly_1, poly_2)))) %>% dplyr::filter(!is.na(.data$X))


  # TIE EVERYTHING UP AND MAKE IT AN SF OBJECT
  poly_full <- sf::st_sf("location" = paste(orientation[1], "-", orientation[2]),
                        sf::st_sfc(sf::st_polygon(list(as.matrix(no_NAs))),
                        crs = crs))

  # print this messages even though we are fixing the problem internally - but we want to use this opportunity to raise the user's awareness that his geographic objects might be invalid
  if (sf::st_is_valid(poly_full) == FALSE) poly_full <- sf::st_make_valid(poly_full)
  if (sf::st_is_valid(poly_full) == FALSE) message("Polygon invalid, have yout tried switching the position of the two endpoints? \n If you think it is a false flag, try to make it valid with sf::st_make_valid().\n")

  # HERE WE COULD ADD, to you want the negation of the result? and then just do cookie cutting with the polygon of the full bbox


  poly_full
}

# corrected T/F
# kept one needed warning message
# removed dontrun{} for the example

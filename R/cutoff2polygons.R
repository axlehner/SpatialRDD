



#' Create treated polygon from (placebo) line
#'
#' Creates an approximation of a "treated/untreated polygon" to assign the status again to each observation after the border has been shifted
#'
#' @param data study dataset to determine the bounding box (so that all observations are covered by the new polygons) in sf format
#' @param cutoff sf object of the (placebo) cut-off
#' @param orientation how is the discontinuity oriented (e.g. "west-west" means both ends of the line go to the western end of the bounding box)
#' @param endpoints at what position on the edge should each polygon end (between 0 and 1, where 0.5 e.g. means right in the middle of the respective edge)
#' @param upside deprecated, this is replaced by doing an intersect with the bounding box to get the difference
#' @param crs epsg number if project crs
#' @param corners clockwise 1-4 (numbers refer to quadrant corners i.e. 1 is top right). 2 corners max. If you need 3 go the other way round with 1 and take the spatial difference with the bounding box afterwards.
#'
#' @return
#' @export
#'
#' @examples
cutoff2polygons <- function(data = bbox, cutoff = lines.sf,
                            orientation = NA, endpoints = c(0, 0), corners = NA,
                            difference = F, upside = T, crs = crs) {

  # TODO
  # - FIX the loop issue: the condition has length > 1 and only the first element will be used

  if (is.na(orientation)) {cat("Please provide the orientation of the border for a better approximation.\n")
    return()
    }

  if (is.na(corners) | corners[1] == 0) {cat("\n No corners selected, both extensions will end in the same side.\n")
    corners <- c(0)
    poly_c_start <- c(NA, NA)
    }

  offsetX <- 0
  offsetY <- 0

  # STARTING CORNER
  if (corners[1] == 1) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])}
  if (corners[1] == 2) {poly_c_start <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])}
  if (corners[1] == 3) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])}
  if (corners[1] == 4) {poly_c_start <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])}
  # this is not used if no corner was given, above it was defaulted to a vector with a 0 then

  # IF 2nd CORNER SPECIFIED: (going clockwise)
  # we don't give you the choice here, the first corner uniquely defines the 2nd
  # so if 2nd element is non.NA we take the next corner point of the bounding box
  if (corners[1] == 1 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymin'])} # 2nd after 1st
  if (corners[1] == 2 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymin'])} # 3rd after 2nd
  if (corners[1] == 3 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmin'], sf::st_bbox(data)['ymax'])} # 4th after 3rd
  if (corners[1] == 4 & !is.na(corners[2])) {poly_c_2nd <- c(sf::st_bbox(data)['xmax'], sf::st_bbox(data)['ymax'])} # 1st after 4th



  # HERE WE START THE UPPER PART
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

  # BIND IT TOGETHER WITH THE ACTUAL CUTOFF
  if (is.na(corners[2])) {poly_1 <- poly_c_start %>% rbind(poly_1_bound)} else { # if there is no 2nd corner, we just bind with starting border
                          poly_1 <- rbind(poly_c_start, poly_c_2nd, poly_1_bound) # if 2nd corner specified, bind 2nd corner (clockwise) inbetween
  }

  colnames(poly_1) <- c("X", "Y")
  poly_1 <- poly_1 %>% rbind(as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)])# bind the upper part with the cutoff line


  # HERE WE CONTINUE WITH THE BOTTOM PART
  if (orientation[2] == "North" | orientation[2] == "north") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[2], # % of the X-axis
                            sf::st_bbox(data)['ymax']) # stay on the northern bound
                         )


  } else if (orientation[2] == "East" | orientation[2] == "east") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmax'], # stay on the eastern bound
                            sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) * endpoints[2]) # % of the Y-axis
                          )



  } else if (orientation[2] == "South" | orientation[2] == "south") {
    poly_2_bound <- rbind(c(sf::st_bbox(data)['xmin'] + (sf::st_bbox(data)['xmax'] - sf::st_bbox(data)['xmin']) * endpoints[1], # % of the X-axis
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

  no_NAs <- as.data.frame(list(as.matrix(rbind(poly_1, poly_2)))) %>% filter(!is.na(X))

  # TIE EVERYTHING UP AND MAKE IT AN SF OBJECT
  poly_full <- sf::st_sf("location" = paste(orientation[1], "-", orientation[2]),
                        sf::st_sfc(st_polygon(list(as.matrix(no_NAs))),
                        crs = crs))




}

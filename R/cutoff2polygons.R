



#' Create treated polygon from (placebo) line
#'
#' Creates a very crude approximation of a "treated/untreated polygon" that makes it easier to assign the status again to each observation after the border has been shifted
#'
#' @param data study dataset to determine the bounding box (so that all observations are covered by the new polygons) in sf format
#' @param cutoff sf object of the (placebo) cut-off
#' @param orientation how is the discontinuity oriented (e.g. "west-west" means both ends of the line go to the western end of the bounding box)
#' @param endpoints at what position on the edge should each polygon end (between 0 and 1, where 0.5 e.g. means right in the middle of the respective edge)
#' @param upside upper- or lowerside? run one with T and once with F to cover the full studyarea (e.g. treated and untreated)
#' @param crs epsg number if project crs
#'
#' @return
#' @export
#'
#' @examples
cutoff2polygons <- function(data = bbox, cutoff = lines.sf, orientation = NA, endpoints = c(0, 0), upside = T, crs = crs) {

  # TODO
  # - write also the N-S in the more general way to choose on which fraction of the boundary it ends
  # - make it a parameter of the function ... c(.2, .8)
  # - outline the 4 most common cases (or so)
  # - one option will be to say for each of the ends you have to specify the himmelsrichtung and a % with it

  # creating both the N-S (up and downside) and E-W (left and rightside) polygon

  if (is.na(orientation)) {cat("Please provide the orientation of the border for a better approximation.\n")
    return()}

  offsetX <- 0
  offsetY <- 0

  if (orientation == "north-south" | orientation == "North-South") {

    if (upside == T) {

      polySideUp <- rbind(c(sf::st_bbox(data)['xmax'] + offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY),
                          c(sf::st_bbox(data)['xmin'] - offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY),
                          as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)], # that's the actual border
                          c(sf::st_bbox(data)['xmax'] + offsetX, # here we add one down to the bottom right corner
                            #sf::st_bbox(lines.sf)['ymin'] + offsetY), # this would be the one perpendicular to the right from the lines.f
                            sf::st_bbox(data)['ymin'] + offsetY), # this would be the one perpendicular to the right from the lines.f
                          c(sf::st_bbox(data)['xmax'] + offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY)
      )


      polySideUp <- sf::st_sf("location" = 'sideUp', sf::st_sfc(st_polygon(list(as.matrix(polySideUp))), crs = crs))
      #plot(polySideUp)
      polySideUp

    } else {

      polySideDown <- rbind(c(sf::st_bbox(data)['xmax'] + offsetX,
                              sf::st_bbox(data)['ymin'] - offsetY),
                            c(sf::st_bbox(data)['xmin'] - offsetX,
                              sf::st_bbox(data)['ymin'] - offsetY),
                            c(sf::st_bbox(data)['xmin'] - offsetX,
                              sf::st_bbox(data)['ymax'] + offsetY),
                            as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)],
                            c(sf::st_bbox(data)['xmax'] + offsetX,
                              sf::st_bbox(data)['ymin'] - offsetY))
      polySideDown <- sf::st_sf("location" = 'sideDown', sf::st_sfc(st_polygon(list(as.matrix(polySideDown))), crs = crs))
      #plot(polySideDown)
      polySideDown

    } # NORTH - SOUTH END
  } else if (orientation == "west-west" | orientation == "West-West") {

    if (upside == T) {

      polySideUp <- rbind(c(sf::st_bbox(data)['xmax'] + offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY), # top right corner
                          c(sf::st_bbox(data)['xmin'] - offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY), # top left corner
                          c(sf::st_bbox(data)['xmin'] - offsetX, # new point, left borderside together with
                            (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) # ymin plus something...
                             * .8 )), #... 80% of the y axis bbox
                          as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)], # that's the actual border
                          c(sf::st_bbox(data)['xmin'] - offsetX, # new point, left borderside together with
                            (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) # ymin plus something...
                             * .2 )),
                          c(sf::st_bbox(data)['xmin'] + offsetX,
                            sf::st_bbox(data)['ymin'] + offsetY), # then we have to go down to the bottom left corner
                          c(sf::st_bbox(data)['xmax'] + offsetX,
                            sf::st_bbox(data)['ymin'] + offsetY), # back to the bottom right corner
                          c(sf::st_bbox(data)['xmax'] + offsetX,
                            sf::st_bbox(data)['ymax'] + offsetY) # back up to the top right corner
      )
      polySideUp <- sf::st_sf("location" = 'sideUp', sf::st_sfc(st_polygon(list(as.matrix(polySideUp))), crs = crs))
      polySideUp
    } else {

      # for the reverse for the Goa "E-W U" we just neeed to gou to the prolonged points
      polySideDown <- rbind(c(sf::st_bbox(data)['xmin'] - offsetX, # new point, left borderside together with
                              (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) # ymin plus something...
                               * .2 )), # start right at the prolongued point instead of bottom right
                            c(sf::st_bbox(data)['xmin'] - offsetX,
                              (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) # ymin plus something...
                               * .8 )),
                            as.data.frame(sf::st_coordinates(cutoff))[, c(1, 2)],
                            # now we go down again to the lower prolongued point
                            c(sf::st_bbox(data)['xmin'] - offsetX, # new point, left borderside together with
                              (sf::st_bbox(data)['ymin'] + (sf::st_bbox(data)['ymax'] - sf::st_bbox(data)['ymin']) # ymin plus something...
                               * .2 ))
      )
      polySideDown <- sf::st_sf("location" = 'sideDown', sf::st_sfc(st_polygon(list(as.matrix(polySideDown))), crs = crs))
      polySideDown
    }
  } # East-West end


}

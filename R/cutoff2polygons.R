



cutoff2polygons <- function(data = bbox, cutoff = lines.sf, crs = crs, upside = T) {

  # creating both the up and downside polygon

  offsetX <- 0
  offsetY <- 0

  if (upside == T) {

    polySideUp <- rbind(c(st_bbox(data)['xmax'] + offsetX,
                          st_bbox(data)['ymax'] + offsetY),
                        c(st_bbox(data)['xmin'] - offsetX,
                          st_bbox(data)['ymax'] + offsetY),
                        as.data.frame(st_coordinates(cutoff))[, c(1, 2)], # that's the actual border
                        c(st_bbox(data)['xmax'] + offsetX, # here we add one down to the bottom right corner
                          #st_bbox(lines.sf)['ymin'] + offsetY), # this would be the one perpendicular to the right from the lines.f
                          st_bbox(data)['ymin'] + offsetY), # this would be the one perpendicular to the right from the lines.f
                        c(st_bbox(data)['xmax'] + offsetX,
                          st_bbox(data)['ymax'] + offsetY)
    )


    polySideUp <- st_sf("id" = 'sideUp', st_sfc(st_polygon(list(as.matrix(polySideUp))), crs = crs))
    #plot(polySideUp)
    polySideUp
  } else {

    polySideDown <- rbind(c(st_bbox(data)['xmax'] + offsetX,
                            st_bbox(data)['ymin'] - offsetY),
                          c(st_bbox(data)['xmin'] - offsetX,
                            st_bbox(data)['ymin'] - offsetY),
                          c(st_bbox(data)['xmin'] - offsetX,
                            st_bbox(data)['ymax'] + offsetY),
                          as.data.frame(st_coordinates(cutoff))[, c(1, 2)],
                          c(st_bbox(data)['xmax'] + offsetX,
                            st_bbox(data)['ymin'] - offsetY))
    polySideDown <- st_sf("id" = 'sideDown', st_sfc(st_polygon(list(as.matrix(polySideDown))), crs = crs))
    #plot(polySideDown)
    polySideDown
  }


}

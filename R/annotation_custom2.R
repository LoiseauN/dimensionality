annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, 
                                ymax = Inf, data){ 
  
  ggplot2::layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                 geom = ggplot2:::GeomCustomAnn, inherit.aes = TRUE, 
                 params = list(grob = grob, xmin = xmin, xmax = xmax, 
                               ymin = ymin, ymax = ymax))
}

my_arrow <- function(...) {
  grid::segmentsGrob(..., arrow = arrow(type = "closed", angle = 5, 
                                        length = unit(5, "mm")))
}
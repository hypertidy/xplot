#' Plot easy stuff
#'
#' Work around core funs being bound to assumptions, we just want points, lines, rects, segments, text, when we want it. :)
#'
#'
#'
#' @inheritParams graphics::plot.default
#' @return nothing, called for side effect (creating or drawing on a plot)
#' @export
#'
#' @examples
#' xplot(1:10, 10:1, type = "b")
#' xplot(rnorm(10), rnorm(10), type = "b", add = T)
#' xrect(runif(36), runif(36), add = FALSE)
#'
xplot <- function(x, y = NULL, ..., add = FALSE, type = "p") {
  args <- list(...)

  if (!add) {

    switch(type,
     p =     plot(x, y, ...),
     l =     plot(x, y, ..., type = type),
     b =     plot(x, y, ..., type = type),
     c =     plot(x, y, ..., type = type),
     o =     plot(x, y, ..., type = type),
     h =     plot(x, y, ..., type = type),
     s =     plot(x, y, ..., type = type),
     S =     plot(x, y, ..., type = type),
     n =    plot(x, y, ..., type = type),
     seg = xsegments(x, y, ..., add = FALSE),
     r = xrect(x, y, ..., add = FALSE),
     t = xtext(x, y, ..., add = FALSE)
    )
  } else {

    x <- xy.coords(x, y)
    switch(type,
           p = points(x, y = NULL,  ...),
           l = lines(x,  y = NULL, ...),
           b = {par(new = TRUE); plot(x, y = NULL, ..., axes = FALSE)},
          ## h = , s = , S =
           n = NULL,

          ## xplot types, up for negotiation

          ## xsegments must get x00,x01,x10,x11 i.e. paired xs in one vector, same for y
          seg = xsegments(x, y = NULL, ..., add = TRUE),
          ## now it's paired xs, ys again
          r = xrect(x, y = NULL, ..., add = TRUE),
          t = xtext(x, y = NULL, ..., add = TRUE)
           )
  }
}

xpoints <- function(x, y = NULL, type = "p", ..., add = FALSE) {

}

xtext <- function(x, y = NULL, type = "t", ..., add = FALSE) {

}
xsegments <- function(x, y = NULL, border = NULL, col = NA, ..., alpha = 1, lty = 1, lwd = 1, asp = "", add = TRUE) {

}
xrect <- function(x, y = NULL, ..., add = FALSE) {
  x <- do.call(cbind, xy.coords(x, y)[c("x", "y")])
  idx <- seq(1, nrow(x), by = 2)
  x0 <- x[idx, ]
  x1 <- x[idx + 1, ]
  ## have to process the dots
  if (!add) plot(range(x[,1]), range(x[,2]), type = "n", ...)
  rect(x0[,1], x0[,2], x1[,1], x1[,2], ...)
}
xquad <- function(x, y = NULL, border = NULL, col = NA, ..., alpha = 1, lty = 1, lwd = 1, asp = "", add = TRUE) {
  xy <- xy.coords(x, y)
  xy$id <- rep(seq_len(length(xy$x)%/%4), each = 4L, length.out = length(xy$x))
  if (!length(xy$x) %% 4 == 0) {
    message("number of coordinates is not divisible by 4, result may fail, or be non-sensible")
  }

  if ( names(dev.cur()) == "null device" || !add) plot(x, y, asp = asp, type = "n", axes = F, xlab = "", ylab = "")
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$inner, vps$figure, vps$plot)

  grid::grid.polygon(xy$x, xy$y, xy$id, gp = grid::gpar(col = border, fill = col, alpha = alpha, lwd = lwd, lty = lty),
                     default.units = "native")


  grid::popViewport(3)

}

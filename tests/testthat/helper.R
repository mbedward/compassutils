#' Create an sf point feature from coordinates
#'
#' @param x Either a two-element vector of coordinates for a single point, or a
#'   two-column matrix of coordinates (column order X,Y) for one or more points.
#'
#' @param crs An optional coordinate reference system for the points specified
#'   as an integer EPSG code, a WKT string or an \code{sf crs} class object.
#'
#' @return A geometry list (\code{sf sfc} class) containing point geometries.
#'
#' @noRd
#'
make_point_features <- function(x, crs = NA) {
  varname <- deparse1(substitute(x))

  stopifnot(is.numeric(x))

  crs <- sf::st_crs(crs)

  if (is.vector(x)) {
    .do_check(checkmate::check_numeric,
              args = list(x = x, len = 2, any.missing = FALSE, finite = TRUE),
              varname = varname)

    x <- matrix(x, nrow = 1)
  }

  .do_check(checkmate::check_matrix,
            args = list(x = x, ncols = 2, any.missing = FALSE),
            varname = varname)

  pts <- lapply(seq_len(nrow(x)), function(k) {
    sf::st_point(x[k,])
  })

  sf::st_sfc(pts, crs = crs)
}

#' Test if an arbitrary object is some type of simple feature
#'
#' Tests if an object is one of the simple feature types known to the \code{sf}
#' package. It does this by attempting to query the geometry type of the object
#' via \code{\link[sf]{st_geometry_type}} and checking whether this results in
#' an error (indicating a non-sf object).
#'
#' @param x The object to test
#'
#' @return \code{TRUE} if \code{x} is a simple feature object of some kind.
#'
#' @examples
#' coords <- c(305170,	6190800)
#' p <- sf::st_point(coords)
#'
#' is_sf_object(coords)  # FALSE
#' is_sf_object(p)  #TRUE
#'
#' @export
#'
is_sf_object <- function(x) {
  tryCatch({t <- sf::st_geometry_type(x, by_geometry = FALSE); TRUE}, error = function(e) FALSE)
}

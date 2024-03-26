# Private helper function for is_upwind()
#
# Takes an object representing point locations which should be a vector (1
# point), a matrix or an sf object of some kind.
#
# Returns a list with named elements 'coords' (matrix) and 'crs' (sf crs object)
#
.points_as_matrix <- function(x) {
  varname <- deparse1(substitute(x))

  x_CRS <- sf::st_crs(NA)

  # Note: check for sf object classes before the is.numeric() check (below)
  # because POINT objects ('sfg') are numeric vectors
  #
  if (is_sf_object(x)) {
    # Some sort of sf spatial data object
    gtype <- sf::st_geometry_type(x, by_geometry = FALSE)
    if (gtype != "POINT") {
      stop("Point data provided as an 'sfc' or 'sf' object must have 'POINT' geometry type")
    }

    x_CRS <- sf::st_crs(x)

    # Get point coordinates as a matrix
    x <- sf::st_coordinates(x)[, 1:2, drop = FALSE]

  } else if (is.numeric(x)) {
    # vector or matrix
    if (is.vector(x)) {
      .do_check(checkmate::check_numeric,
                args = list(x = x, len = 2, any.missing = FALSE, finite = TRUE),
                varname = varname)

      x <- matrix(x, nrow = 1)
    }

    .do_check(checkmate::check_matrix,
              args = list(x = x, ncols = 2, any.missing = FALSE),
              varname = varname)

  } else {
    # Something other than point coordinates or an sf object was provided
    msg <- glue::glue("Invalid type for point data argument {varname}: {class(x)}")
    stop(msg)
  }

  # Return point coordinates matrix with the CRS, whether known or missing,
  # as an attribute
  attr(x, "crs") <- x_CRS

  x
}


# Private helper function to run a checkmate::check_* function with a given set
# of arguments. Its main purpose is to provide an error message with a specified
# variable name when called from some way down a chain of functions.
#
.do_check <- function(fn, args, varname) {
  res <- do.call(fn, args)
  if (isTRUE(res)) {
    TRUE
  } else {
    msg <- glue::glue("Problem with argument {varname}: {res}")
    stop(msg)
  }
}


# Private function to convert a two-column matrix of coordinates to a list of
# point geometries. No CRS is applied or assumed.
#
.matrix_to_points <- function(mat) {
  stopifnot(ncol(mat) == 2)

  p <- lapply(seq_len(nrow(mat)), function(i) {
    sf::st_point(mat[i,])
  })

  sf::st_sfc(p)
}


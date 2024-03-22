#' Convert angular values from radians to degrees
#'
#' All input values must be finite. Missing values are allowed and will simply
#' be returned as \code{NA}.
#'
#' @param x A numeric vector of one or more finite angular values expressed in
#'   radians.
#'
#' @return A numeric vector of angular values in degrees.
#'
#' @export
#
rad2deg <- function(x) {
  checkmate::assert_numeric(x, finite = TRUE)
  180 * x / pi
}


#' Convert angular values from degrees to radians
#'
#' All input values must be finite. Missing values are allowed and will simply
#' be returned as \code{NA}.
#'
#' @param x A numeric vector of one or more finite angular values expressed in
#'   degrees.
#'
#' @return A numeric vector of angular values in radians.
#'
#' @export
#
deg2rad <- function(x) {
  checkmate::assert_numeric(x, finite = TRUE)
  (x * pi) / 180
}

#' Convert a compass bearing to a Cartesian angle
#'
#' Compass bearings range from north (0 degrees) through east (90), south (180)
#' and west (270). In contrast, Cartesian angles define 0 degrees as the
#' direction of the positive X-axis and then proceed counter-clockwise such that
#' 90 degrees (pi/2 radians) is the direction of the positive Y-axis etc.
#'
#' @param x A numeric vector of one or more compass bearings. By default, it is
#'   assumed that the angular units are degrees. See the \code{degrees} argument
#'   if input values should be treated as radians.
#'
#' @param degrees (logical) If \code{TRUE} (default), input values are treated
#'   as compass angles in degrees. Set to \code{FALSE} if input values should be
#'   treated as radians.
#'
#' @return A numeric vector of corresponding Cartesian angles, with angular
#'   units the same as those of the input values. The returned values will be in
#'   the range \code{[0, 360)} when units are degrees, or \code{[0, 2*pi)} when
#'   units are radians.
#'
#' @examples
#' # Convert a north-west compass bearing (315 degrees) to the corresponding
#' # Cartesian angle expressed in radians (3*pi/4 = 2.356194)
#' #
#' compass2cartesian(315) |> deg2rad()
#'
#' @seealso [cartesian2compass()]
#'
#' @export
#'
compass2cartesian <- function(x, degrees = TRUE) {
  checkmate::assert_numeric(x, finite = TRUE)

  # Work in degrees because that's easiest to think about
  if (!degrees) x <- rad2deg(x)
  x <- (90 - x) %% 360

  if (!degrees) x <- deg2rad(x)

  x
}


#' Convert a Cartesian angle to a compass bearing
#'
#' Compass bearings range from north (0 degrees) through east (90), south (180)
#' and west (270). In contrast, Cartesian angles define 0 degrees as the
#' direction of the positive X-axis and then proceed counter-clockwise such that
#' 90 degrees (pi/2 radians) is the direction of the positive Y-axis etc.
#'
#' The conversion between compass and cartesian coordinates is symmetric, so
#' this function is really just a wrapper that calls \code{compass2cartesian}.
#' It is just provided to make the intention of client code more obvious.
#'
#' @param x A numeric vector of one or more Cartesian angles. By default, it is
#'   assumed that the angular units are degrees. See the \code{degrees} argument
#'   if input values should be treated as radians.
#'
#' @param degrees (logical) If \code{TRUE} (default), input values are treated
#'   as Cartesian angles in degrees. Set to \code{FALSE} if input values should
#'   be treated as radians.
#'
#' @return A numeric vector of corresponding compass angles, with angular
#'   units the same as those of the input values. The returned values will be in
#'   the range \code{[0, 360)} when units are degrees, or \code{[0, 2*pi)} when
#'   units are radians.
#'
#' @examples
#' # Convert a Cartesian angle of -pi/4 radians to the corresponding compass bearing
#' # expressed in degrees (135 degrees)
#' #
#' cart_radians <- -pi/4
#' cartesian2compass(cart_radians, degrees = FALSE) |> rad2deg()
#'
#' @seealso [compass2cartesian()]
#'
#' @export
#'
cartesian2compass <- function(x, degrees = TRUE) {
  # Calculations are exactly the same as when converting the other way
  compass2cartesian(x, degrees = degrees)
}


#' Determine if a point location is up-wind of a reference point
#'
#' Given a reference point location \code{p0} and one or more query point
#' locations \code{pquery}, together with a prevailing wind direction, this
#' function determines whether each query point is up-wind of the reference
#' point. To be up-wind, a query point must lie strictly within an angular range
#' centred on the direction from which the wind is coming. By default, the range
#' is set to 90 degrees either side of the wind direction, but this can be
#' decreased for a stricter interpretation of 'up-wind'. To accord with the
#' standard meteorological convention, the wind direction value
#' \code{wdir_degrees} should always be expressed as a compass bearing in
#' degrees, representing the direction from which the wind is blowing, e.g. 270
#' degrees for a westerly wind with air flow from west to east.
#'
#' Note that this function \strong{assumes} that wind direction and the range
#' up-wind bearings are both expressed in degrees.
#'
#' @param p0 The reference point. Either a two-element vector of X-Y
#'   coordinates, an \code{sf} point geometry object, a single element from an
#'   \code{sfc} point geometry list or a single record from an \code{sf} spatial
#'   data frame.
#'
#' @param pquery One or more query points. If there is only query point it can
#'   be provided in the same forms as described for \code{p0}. Multiple query
#'   points can be provided as a matrix of X-Y coordinates, an \code{sfc} point
#'   geometry list or an \code{sf} spatial data frame.
#'
#' @param wdir_degrees Wind direction (single numeric value), expressed using
#'   the standard meteorological convention as the compass direction from which
#'   the wind is coming, e.g. 270 degrees means a westerly wind with air flow
#'   from west to east.
#'
#' @param halfspan The angular range (degrees) either side of the wind angle for
#'   a bearing to be considered up-wind. Must be in the range \code{[0, 180]}.
#'   The default value is 90 degrees for a semi-circle centred on the wind
#'   direction.
#'
#' @export
#
point_is_upwind <- function(p0, pquery, wdir_degrees, halfspan = 90) {
  checkmate::assert_number(wdir_degrees, finite = TRUE)
  checkmate::assert_number(halfspan, finite = TRUE, lower = 0, upper = 180)

  res <- .points_as_matrix(p0)
  p0_coords <- res$coords
  if (nrow(p0_coords) != 1) stop("There should only be one reference point (p0)")

  # Store p0 as a vector rather than a matrix
  p0_coords <- c(p0_coords)

  p0_CRS <- res$crs

  res <- .points_as_matrix(pquery)
  pquery_coords <- res$coords
  pquery_CRS <- res$crs

  upwind <- sapply(seq_len(nrow(pquery_coords)), function(i) {
    dir <- get_compass_direction(p0_coords, pquery_coords[i,])
    angle_in_range(dir, wdir_degrees, halfspan, degrees = TRUE)
  })

  upwind
}


#' Test if angles are within a given angular range
#'
#' This function is intended to help with queries such as 'is a given compass
#' bearing within x degrees of west' or 'is a given location markedly up-wind of
#' our present position'. Given one or more input angles (e.g. compass
#' bearings), it determines whether each lies within an angular range centred on
#' the angle \code{mid} and ranging \code{halfspan} angular units either side.
#' By default, the function assumes angular units are degrees for working with
#' compass bearings. Set the \code{degrees} argument to \code{FALSE} if working
#' with values in radians.
#'
#' @param x A numeric vector of one or more angles to check.
#'
#' @param mid A single numeric value for the middle angle of the sector.
#'
#' @param halfspan A single numeric value in the range \code{[0, 360]} (if
#'   \code{degrees=TRUE}) or \code{[0, 2*pi]} (if \code{degrees=FALSE})
#'   representing the angular range either side of \code{mid}. Normally this
#'   will be a positive value representing a finite angular range. A zero value
#'   is allowed although this would simply result in an equality check between
#'   the input \code{x} angles and the \code{mid} angle. Any value smaller than
#'   \code{sqrt(.Machine$double.eps)} will be treated as zero.
#'
#' @param degrees (logical) If \code{TRUE} (default), all values are treated as
#'   angles in degrees. Set to \code{FALSE} if values should be treated as
#'   radians.
#'
#' @return A logical vector with an element for each input angle, where
#'   \code{TRUE} indicates the angle lies within the sector.
#'
#' @examples
#' # Determine which of a set of compass bearings represent up-wind directions,
#' # given a prevailing north-westerly wind (315 degrees) and treating
#' # 'up-wind' as any bearing within 60 degrees either side of the wind
#' # direction.
#' #
#' bearings <- seq(0, 359, 22.5)
#' upwind <- angle_in_range(bearings, 315, 60)
#'
#' # display input bearings and up-wind status as a data frame
#' data.frame(bearings, upwind)
#'
#' @export
#'
angle_in_range <- function(x, mid, halfspan, degrees = TRUE) {
  checkmate::assert_numeric(x, finite = TRUE)
  checkmate::assert_number(mid, finite = TRUE)
  checkmate::assert_flag(degrees)

  if (degrees) {
    x <- deg2rad(x)
    mid <- deg2rad(mid)
    halfspan <- deg2rad(halfspan)
  }

  checkmate::assert_number(halfspan, finite = TRUE, lower = 0, upper = 2*pi)

  cos(x - mid) > cos(halfspan)
}


#' Determine the compass bearing from one point to another
#'
#' Given a reference point p0 and a query point p1, each represented by a
#' two-element vector of X:Y coordinates, find the compass bearing in degrees of
#' p1 from reference point p0.
#'
#' @param p0 A two-element numeric vector of X-Y coordinates for the reference
#'   point.
#'
#' @param p1 A two-element numeric vector of X-Y coordinates for the query
#'   point.
#'
#' @return The compass bearing of p1 from p0 expressed in degrees, or \code{NA}
#'   if the two points are the same.
#'
#' @examples
#' p0 <- c(700000, 6530000)
#' p1 <- c(695000, 6535000) # a point north-west of p0
#' get_compass_bearing(p0, p1)  # returns 315 degrees
#'
#' # If the two points are the same there is no valid direction
#' get_compass_bearing(p0, p0)  # returns NA
#'
#' @export
#
get_compass_bearing <- function(p0, p1) {
  checkmate::assert_numeric(p0, len = 2, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(p1, len = 2, any.missing = FALSE, finite = TRUE)

  # Check for degenerate case
  if (isTRUE(all.equal(p0, p1, check.attributes = FALSE))) {
    NA_real_
  } else {
    dxy <- unname(p1 - p0)

    # Cartesian angle in radians
    angle <- atan2(y = dxy[2], x = dxy[1])

    # Compass angle in degrees
    rad2deg( cartesian2compass(angle, degrees = FALSE) )
  }
}


# Private helper function for is_upwind()
#
# Takes an object representing point locations which should be a vector (1
# point), a matrix or an sf object of some kind. Records the CRS (if an sf
# object) and retrieves point coordinates as a matrix.
#
.points_as_matrix <- function(x) {
  varname <- deparse1(substitute(x))

  x_CRS <- sf::st_crs(NA)

  if (is.numeric(x)) {
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

  } else if (inherits(x, "sfc") || inherits(x, "sf")) {
    # Some sort of sf spatial data object
    if (!all(sf::st_geometry_type(x) == "POINT")) {
      stop("Point data provided as an 'sfc' or 'sf' object must have 'POINT' geometry type")
    }

    x_CRS <- sf::st_crs(x)

    # Get point coordinates as a matrix
    x <- sf::st_coordinates(x)[, 1:2]

  } else {
    # Something other than point coordinates or an sf object was provided
    msg <- glue::glue("Invalid type for point data argument {varname}: {class(x)}")
    stop(msg)
  }

  # Return point coordinates and (possibly) the CRS
  list(coords = x, crs = x_CRS)
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


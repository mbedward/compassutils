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


#' Determine the compass bearing from one point to one or more other points
#'
#' Given a reference point \code{p0} and one or more query points \code{pquery},
#' find the compass bearing in degrees of each query point from \code{p0}.
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
get_compass_bearing <- function(p0, pquery) {
  p0_coords <- .points_as_matrix(p0)
  if (nrow(p0_coords) != 1) stop("There should only be one reference point (p0)")

  p0_CRS <- attr(p0_coords, "crs")

  # Store p0 as a vector rather than a matrix
  p0_coords <- c(p0_coords)

  pquery_coords <- .points_as_matrix(pquery)
  pquery_CRS <- attr(pquery_coords, "crs")

  sapply(seq_len(nrow(pquery_coords)), function(i) {
    .get_compass_bearing_coords(p0_coords, pquery_coords[i,])
  })
}


# Private helper function for get_compass_bearing()
#
.get_compass_bearing_coords <- function(p0_coords, p1_coords) {
  checkmate::assert_numeric(p0_coords, len = 2, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(p1_coords, len = 2, any.missing = FALSE, finite = TRUE)

  # Check for degenerate case
  if (isTRUE(all.equal(p0_coords, p1_coords, check.attributes = FALSE))) {
    NA_real_
  } else {
    dxy <- unname(p1_coords - p0_coords)

    # Cartesian angle in radians
    angle <- atan2(y = dxy[2], x = dxy[1])

    # Compass angle in degrees
    rad2deg( cartesian2compass(angle, degrees = FALSE) )
  }
}


#' Determine if point locations are up-wind of a reference point
#'
#' Given a single reference point location \code{p0} and one or more query point
#' locations \code{pquery}, together with a prevailing wind direction, determine
#' whether each query point is up-wind of the reference point. This function is
#' simply a short-cut to calling functions \code{get_compass_bearing()} and
#' \code{angle_in_range()}. To be up-wind, a query point must lie within an
#' angular range centred on the direction from which the wind is coming. By
#' default, the range is set to 90 degrees either side of the wind direction,
#' but this can be decreased for a stricter interpretation of 'up-wind'. To
#' accord with the standard meteorological convention, the wind direction value
#' \code{wdir_degrees} should always be expressed as a compass bearing in
#' degrees, representing the direction from which the wind is blowing, e.g. 270
#' degrees for a westerly wind with air flow from west to east.
#'
#' Note that this function \strong{assumes} that wind direction, \code{wdir},
#' and the \code{halfspan} arguments are both expressed in degrees.
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
#' @param strict (logical; default FALSE) Whether bearings must be strictly
#'   within the angular range defined by \code{halfspan}. See
#'   \code{\link{angle_in_range}} for more explanation.
#'
#' @export
#
is_upwind <- function(p0, pquery, wdir_degrees, halfspan = 90, strict = FALSE) {
  checkmate::assert_number(wdir_degrees, finite = TRUE)
  checkmate::assert_number(halfspan, finite = TRUE, lower = 0, upper = 180)
  checkmate::assert_flag(strict)

  p0_coords <- .points_as_matrix(p0)
  if (nrow(p0_coords) != 1) stop("There should only be one reference point (p0)")

  p0_CRS <- attr(p0_coords, "crs")

  # Store p0 as a vector rather than a matrix
  p0_coords <- c(p0_coords)

  pquery_coords <- .points_as_matrix(pquery)
  pquery_CRS <- attr(pquery_coords, "crs")

  sapply(seq_len(nrow(pquery_coords)), function(i) {
    b <- get_compass_bearing(p0_coords, pquery_coords[i,])
    angle_in_range(b, wdir_degrees, halfspan, degrees = TRUE, strict = strict)
  })
}


#' Test if one or more angles lie strictly within a given angular range
#'
#' This function is intended to help with queries such as 'is a given compass
#' bearing within x degrees of west' or 'is a given location markedly up-wind of
#' our present position'. Given one or more input angles (e.g. compass
#' bearings), it determines whether each lies within an angular range centred on
#' the angle \code{mid} and extended for \code{halfspan} angular units either
#' side. The end-points of the range (i.e. \code{mid +- halfspan}) are treated
#' as outside by default. To treat them as included, set \code{strict=FALSE}.
#'
#' @param x A numeric vector of one or more angles to check.
#'
#' @param mid A single numeric value for the middle angle of the sector.
#'
#' @param halfspan A single numeric value in the range \code{[0, 180]} (if
#'   \code{degrees=TRUE}) or \code{[0, pi]} (if \code{degrees=FALSE})
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
#' @param strict (logical) If \code{TRUE}, the end-points of the angular range
#'   are treated as outside, thus the function is testing if the input angles
#'   are less than \code{halfspan} units from \code{mid}. If \code{FALSE}
#'   (default), the end-points are treated as being within the
#'   range, thus the function is testing if the input angles are \emph{no more
#'   than} \code{halfspan} units from \code{mid}. Note that in the degenerate case where \code{halfspan=0},
#'   setting \code{strict=TRUE} would mean that the function always returns
#'   \code{FALSE}, even if the input angle equals \code{mid}.
#'
#' @return A logical vector with an element for each input angle, where
#'   \code{TRUE} indicates the angle lies within the sector.
#'
#' @examples
#' # Determine which of a set of compass bearings represent up-wind directions,
#' # given a prevailing north-westerly wind (315 degrees) and treating
#' # 'up-wind' as any bearing strictly within 60 degrees either side of the wind
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
angle_in_range <- function(x, mid, halfspan, degrees = TRUE, strict = FALSE) {
  checkmate::assert_numeric(x, finite = TRUE)
  checkmate::assert_number(mid, finite = TRUE)
  checkmate::assert_flag(degrees)
  checkmate::assert_flag(strict)

  # upper bound on halfspan
  if (degrees) max_upper <- 180
  else max_upper <- pi

  checkmate::assert_number(halfspan, finite = TRUE, lower = 0, upper = max_upper)

  if (halfspan < sqrt(.Machine$double.eps)) halfspan <- 0

  if (degrees) {
    x <- deg2rad(x)
    mid <- deg2rad(mid)
    halfspan <- deg2rad(halfspan)
  }

  if (strict) cos(x - mid) > cos(halfspan)
  else cos(x - mid) >= cos(halfspan)
}


#' Find a target point at a given bearing and distance from a reference point
#'
#' Given a reference point \code{p0}, find the coordinates of a target point
#' located at a given compass bearing and distance from \code{p0}.
#'
#' @note This function currently only useful for points in projected coordinate
#'   systems, not geographic (longitude, latitude) coordinates.
#'
#' @param p0 The reference point. Either a two-element vector of X-Y
#'   coordinates, an \code{sf} point geometry object, a single element from an
#'   \code{sfc} point geometry list or a single record from an \code{sf} spatial
#'   data frame.
#'
#' @param bearing The compass direction from p0 to the target point.
#'
#' @param distance Distance to the new point. Generally, this will be a positive
#'   value. If zero, the function simply returns the reference point
#'   coordinates. If negative, a point in the opposite direction to that
#'   specified by the \code{bearing} will be returned.
#'
#' @param degrees (logical) If \code{TRUE} (default), all bearings are treated
#'   as compass angles in degrees. Set to \code{FALSE} if values should be
#'   treated as radians.
#'
#' @return A two-element vector with the coordinates of the target point.
#'
#' @examples
#' # Reference point coordinates (assuming map units are metres)
#' p0 <- c(305170, 6190800)
#'
#' # Find the target point 170 metres from the reference point along a compass
#' # bearing of 10 degrees (slightly east of north)
#' #
#' ptarget <- get_target_point(p0, bearing = 10, distance = 170)
#' cat("Target point coordinates:", ptarget)
#'
#' @export
#'
get_target_point <- function(p0, bearing, distance, degrees = TRUE) {
  checkmate::assert_number(bearing, finite = TRUE)
  checkmate::assert_number(distance, finite = TRUE)

  p0 <- .points_as_matrix(p0)
  if (nrow(p0) != 1) stop("Only one reference point (p0) can be provided")

  # If we know the CRS of the reference point, check that it is projected
  p0crs <- attr(p0, "crs")
  if (!is.na(p0crs) && sf::st_is_longlat(p0crs)) {
    stop("Reference point p0 must have projected coordinates, not geographic (lon-lat)")
  }

  # reduce single-row matrix for reference point coords to a 2-element vector
  p0 <- c(p0)

  bcart <- compass2cartesian(bearing, degrees = degrees)

  if (degrees) {
    bcart <- deg2rad(bcart)
  }

  p0 + c(distance * cos(bcart), distance * sin(bcart))
}


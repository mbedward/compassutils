% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/angle_functions.R
\name{cartesian2compass}
\alias{cartesian2compass}
\title{Convert a Cartesian angle to a compass bearing}
\usage{
cartesian2compass(x, degrees = TRUE)
}
\arguments{
\item{x}{A numeric vector of one or more Cartesian angles. By default, it is
assumed that the angular units are degrees. See the \code{degrees} argument
if input values should be treated as radians.}

\item{degrees}{(logical) If \code{TRUE} (default), input values are treated
as Cartesian angles in degrees. Set to \code{FALSE} if input values should
be treated as radians.}
}
\value{
A numeric vector of corresponding compass angles, with angular
units the same as those of the input values. The returned values will be in
the range \code{[0, 360)} when units are degrees, or \code{[0, 2*pi)} when
units are radians.
}
\description{
Compass bearings range from north (0 degrees) through east (90), south (180)
and west (270). In contrast, Cartesian angles define 0 degrees as the
direction of the positive X-axis and then proceed counter-clockwise such that
90 degrees (pi/2 radians) is the direction of the positive Y-axis etc.
}
\details{
The conversion between compass and cartesian coordinates is symmetric, so
this function is really just a wrapper that calls \code{compass2cartesian}.
It is just provided to make the intention of client code more obvious.
}
\examples{
# Convert a Cartesian angle of -pi/4 radians to the corresponding compass bearing
# expressed in degrees (135 degrees)
#
cart_radians <- -pi/4
cartesian2compass(cart_radians, degrees = FALSE) |> rad2deg()

}
\seealso{
\code{\link[=compass2cartesian]{compass2cartesian()}}
}

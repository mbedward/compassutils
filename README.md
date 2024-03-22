[![R-CMD-check](https://github.com/mbedward/compassutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mbedward/compassutils/actions/workflows/R-CMD-check.yaml)

## compassutils

This package provides some simple functions for working with map and compass bearings. The standard trigonometric functions in R (e.g. `sin`, `cos`, `atan2`) assume Cartesian angles expressed in radians where a value of 0 is the direction of the positive X-axis (east on a compass), and positive angles indicate anti-clockwise rotation. So a value of pi/2 radians (90 degrees) is the positive Y-axis direction (compass north) while 3*pi/2 radians (270 degrees) (or equivalently -pi/2 radians) is the negative Y-axis direction (compass south). 

In contrast, compass bearings are almost always expressed in degrees, set 0 as north (positive Y-axis) and proceed clockwise. So east (positive X-axis) is 90 degrees, south is 180 degrees etc. Terrible things can happen when you confuse these two systems! So to help keep me (and you) safe from such confusion, this package provides functions to convert between compass and Cartesian angles, as well as common tasks such as calculating the compass bearing of one point location from another, and testing whether a bearing lies within a sector (i.e. an angular range).

Please note: this package is mainly intended for use by myself and colleagues at the Centre for Environmental Risk Management of Bushfire, University of Wollongong. Everyone is welcome to use it but please treat it as a very early work-in-progress.

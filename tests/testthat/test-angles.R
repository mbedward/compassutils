test_that("radians to degrees", {
  r <- seq(-pi, pi, pi/4)
  d <- seq(-180, 180, 45)

  expect_equal(rad2deg(r), d)
})

test_that("degrees to radians", {
  d <- seq(-180, 180, 45)
  r <- seq(-pi, pi, pi/4)

  expect_equal(deg2rad(d), r)
})

test_that("rad2deg does not allow infinite values", {
  expect_error(rad2deg(c(0, 1, Inf, -1)))
})

test_that("rad2deg passes along missing values", {
  r <- c(0, -pi/4, NA, pi/4, NA)
  d <- c(0, -45, NA, 45, NA)

  expect_equal(rad2deg(r), d)
})

test_that("deg2rad does not allow `infinite values", {
  expect_error(deg2rad(c(0, 1, Inf, -1)))
})

test_that("deg2rad passes along missing values", {
  d <- c(0, -45, NA, 45, NA)
  r <- c(0, -pi/4, NA, pi/4, NA)

  expect_equal(deg2rad(d), r)
})

test_that("can convert compass bearings in normal degrees range to cartesian", {
  comp <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  cart <- c(90, 45, 0, 315, 270, 225, 180, 135, 90)

  expect_equal(compass2cartesian(comp), cart)
})

test_that("can convert cartesian angles in radians to compass bearings", {
  cart_rad <- c(-pi, -3*pi/4, -pi/2, -pi/4, 0, pi/4, pi/2, 3*pi/4, pi)

  comp_deg <- c(270, 225, 180, 135, 90, 45, 0, 315, 270)
  comp_rad <- deg2rad(comp_deg)

  expect_equal(cartesian2compass(cart_rad, degrees = FALSE), comp_rad)
})

test_that("get_compass_direction works", {
  p0 <- c(0, 0)

  # matrix of query points and expected directions from p0
  p1s <- matrix(c(
    0, 1, 0,
    1, 1, 45,
    1, 0, 90,
    1, -1, 135,
    0, -1, 180,
    -1, -1, 225,
    -1, 0, 270,
    -1, 1, 315),
    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("x", "y", "dir")))

  dir <- apply(p1s, MARGIN = 1, FUN = function(x) { get_compass_bearing(p0, x[1:2]) })

  testthat::expect_equal(dir, p1s[,"dir"])
})

test_that("get_compass_direction returns NA when points are the same", {
  p <- c(700000, 6500000)
  dir <- get_compass_bearing(p, p)
  testthat::expect_true(is.na(dir))
})


test_that("rad2deg works", {
  r <- seq(-pi, pi, pi/4)
  d <- seq(-180, 180, 45)

  expect_equal(rad2deg(r), d)
})


test_that("rad2deg handles missing values", {
  r <- seq(-pi, pi, pi/4)

  n <- length(r)
  ina <- sample(n, ceiling(n/2))
  iok <- setdiff(seq_len(n), ina)

  r[ina] <- NA
  d <- rad2deg(r)

  # All missing inputs map to missing outputs
  expect_true( all( is.na(d[ina]) ) )

  # No data inputs map to missing outputs
  expect_false( any( is.na(d[iok]) ) )
})


test_that("rad2deg does not allow infinite values", {
  expect_error(rad2deg(c(0, 1, Inf, -1)))
})


test_that("deg2rad works", {
  d <- seq(-180, 180, 45)
  r <- seq(-pi, pi, pi/4)

  expect_equal(deg2rad(d), r)
})


test_that("deg2rad handles missing values", {
  d <- seq(-180, 180, 45)

  n <- length(d)
  ina <- sample(n, ceiling(n/2))
  iok <- setdiff(seq_len(n), ina)

  d[ina] <- NA
  r <- deg2rad(d)

  # All missing inputs map to missing outputs
  expect_true( all( is.na(r[ina]) ) )

  # No data inputs map to missing outputs
  expect_false( any( is.na(r[iok]) ) )
})


test_that("rad2deg passes along missing values", {
  r <- c(0, -pi/4, NA, pi/4, NA)
  d <- c(0, -45, NA, 45, NA)

  expect_equal(rad2deg(r), d)
})


test_that("deg2rad does not allow `infinite values", {
  expect_error(deg2rad(c(0, 1, Inf, -1)))
})


test_that("can convert bearings in normal degrees range to cartesian", {
  comp <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  cart <- c(90, 45, 0, 315, 270, 225, 180, 135, 90)

  expect_equal(compass2cartesian(comp), cart)
})


test_that("can convert bearings outside normal degree range to cartesian", {
  comp <- c(-45, -90, -135, -180, -225, -270, -315)
  cart <- c(135, 180,  225,  270,  315,    0,   45)

  expect_equal(compass2cartesian(comp), cart)
})


test_that("can convert cartesian angles in radians to compass bearings", {
  cart_rad <- c(-pi, -3*pi/4, -pi/2, -pi/4, 0, pi/4, pi/2, 3*pi/4, pi)

  comp_deg <- c(270, 225, 180, 135, 90, 45, 0, 315, 270)
  comp_rad <- deg2rad(comp_deg)

  expect_equal(cartesian2compass(cart_rad, degrees = FALSE), comp_rad)
})


test_that("get_compass_bearing works with coodinates", {
  p0 <- c(0, 0)

  # matrix of query points and expected directions from p0
  p1s <- matrix(c(
       0,  100,   0,
     100,  100,  45,
     100,    0,  90,
     100, -100, 135,
       0, -100, 180,
    -100, -100, 225,
    -100,    0, 270,
    -100,  100, 315),
    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("x", "y", "bearing")))

  bearing <- apply(p1s, MARGIN = 1, FUN = function(x) { get_compass_bearing(p0, x[1:2]) })

  expect_equal(bearing, p1s[,"bearing"])
})


test_that("get_compass_bearing works with point features", {
  p0 <- c(305170, 6190800)
  p0_sf <- make_point_features(p0)

  dxy <- matrix(c(
       0,  100,   0,
     100,  100,  45,
     100,    0,  90,
     100, -100, 135,
       0, -100, 180,
    -100, -100, 225,
    -100,    0, 270,
    -100,  100, 315),
    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("x", "y", "bearing")))

  p <- dxy[, 1:2] + rep(p0, each = nrow(dxy))
  p_sf <- make_point_features(p)

  bearings <- get_compass_bearing(p0_sf, p_sf)

  expect_equal(bearings, dxy[,"bearing"])
})


test_that("get_compass_bearing works with an sf data frame", {
  p0 <- c(305170, 6190800)

  dxy <- matrix(c(
    0,  100,   0,
    100,  100,  45,
    100,    0,  90,
    100, -100, 135,
    0, -100, 180,
    -100, -100, 225,
    -100,    0, 270,
    -100,  100, 315),
    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("x", "y", "bearing")))

  p <- dxy[, 1:2] + rep(p0, each = nrow(dxy))

  # Make an sf data frame for the query features
  p_geoms <- make_point_features(p)
  pquery <- sf::st_sf(index = seq_len(nrow(dxy)),
                      expected_bearing = dxy[,3],
                      geom = p_geoms)

  # Pass p0 as a numeric vector and pquery as an sf data frame
  bearings <- get_compass_bearing(p0, pquery)

  expect_equal(bearings, pquery$expected_bearing)
})


test_that("get_compass_bearing checks CRS of both inputs", {
  p0 <- c(305170, 6190800)
  p1 <- p0 + c(-1000, 1000)

  f <- function() {
    get_compass_bearing(p0, p1)
  }

  # Case 1: inputs are vectors so no CRS for either input - this is allowed
  expect_no_error(f())

  # Case 2: inputs are point features with no CRS - this is allowed
  p0 <- make_point_features(p0)
  p1 <- make_point_features(p1)
  expect_no_error(f())

  # Case 3: same CRS defined for both features
  sf::st_crs(p0) <- 28356
  sf::st_crs(p1) <- 28356
  expect_no_error(f())

  # Case 4: CRS only defined for one feature - error
  sf::st_crs(p1) <- NA
  expect_error(f())

  # Case 5: different CRSs for the two features - error
  sf::st_crs(p1) <- 7856
  expect_error(f())
})


test_that("get_compass_bearing returns NA when points are the same", {
  p <- c(305170, 6190800)
  dir <- get_compass_bearing(p, p)
  expect_true(is.na(dir))
})


test_that("get_compass_bearing agrees with get_target_point", {
  p0 <- c(305170, 6190800)

  N <- 100
  bearings <- runif(N, 0, 360)

  observed <- sapply(bearings, function(b) {
    p <- get_target_point(p0, bearing = b, distance = 100)
    get_compass_bearing(p0, p)
  })

  expect_equal(observed, bearings)
})


test_that("get_target_point works with zero distance", {
  p0 <- c(305170, 6190800)
  N <- 100
  bearings <- runif(N, 0, 360)

  same_as_p0 <- sapply(bearings, function(b) {
    p <- get_target_point(p0, bearing = b, distance = 0) # note zero distance
    isTRUE(all.equal(p, p0, check.attributes = FALSE))
  })

  expect_true(all(same_as_p0))
})


test_that("get_target_point works with negative distance", {
  p0 <- c(305170, 6190800)
  N <- 100
  bearings <- runif(N, 0, 360)

  ok <- sapply(bearings, function(b) {
    # target point with negative distance
    pneg <- get_target_point(p0, bearing = b, distance = -100)

    # should be same point as positive distance in opposite direction
    pexpected <- get_target_point(p0, bearing = b + 180, distance = 100)

    isTRUE(all.equal(pneg, pexpected, check.attributes = FALSE))
  })

  expect_true(all(ok))
})


test_that("angle_in_range works", {
  mid <- 270
  halfspan <- 45

  # test angles
  x <- c(220, 225, 226, 270, 314, 315, 320)

  # expected with default non-strict comparison
  expected <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  expect_equal(angle_in_range(x, mid, halfspan, degrees = TRUE), expected)

  # expected with strict comparison
  expected <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_equal(angle_in_range(x, mid, halfspan, degrees = TRUE, strict = TRUE), expected)
})


test_that("is_upwind works", {
  # reference point
  p0 <- c(305170, 6190800)

  # wind direction north-west, compass degrees
  wdir <- 315

  # generate points at random bearings from p0 and test each
  # for being up-wind, setting the angular range half-span to 30 degrees
  N <- 100
  halfspan <- 30
  bearings <- runif(N, 0, 360)

  res <- sapply(bearings, function(b) {
    p <- get_target_point(p0, bearing = b, distance = 100)

    expected <- angle_in_range(b, wdir, halfspan = halfspan, strict = TRUE)
    observed <- is_upwind(p0, p, wdir, halfspan = halfspan, strict = TRUE)

    c(observed, expected)
  })

  expect_equal(res[1,], res[2,])
})


test_that("is_upwind works with zero half-span", {
  # reference point
  p0 <- c(305170, 6190800)

  # wind direction north-west, compass degrees
  wdir <- 315

  # with zero half-span, only a point on the same bearing as the wind
  # direction should be seen as up-wind
  p <- get_target_point(p0, bearing = wdir, distance = 100)

  observed <- is_upwind(p0, p, wdir, halfspan = 0, strict = FALSE)
  expect_true(observed)

  # but if strict=TRUE the function will always return FALSE with
  # zero half-span
  observed <- is_upwind(p0, p, wdir, halfspan = 0, strict = TRUE)
  expect_false(observed)
})

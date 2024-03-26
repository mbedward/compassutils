test_that(".do_check private helper function works", {
  x_2elements <- 1:2

  res <- .do_check(checkmate::check_numeric,
                   args = list(x = x_2elements, len = 2, any.missing = FALSE, finite = TRUE),
                   varname = "x_2elements")

  expect_true(res)

  x_3elements <- 1:3

  expect_error(
    .do_check(checkmate::check_numeric,
              args = list(x = x_3elements, len = 2, any.missing = FALSE, finite = TRUE),
              varname = "x_3elements"),

    "Problem with argument x_3elements.+length",
    ignore.case = TRUE)
})


test_that(".points_as_matrix accepts vector for a single point", {
  p <- c(305170,	6190800)
  res <- .points_as_matrix(p)

  expect_true(is.matrix(res))
  expect_equal(dim(res), c(1,2))

  crs <- attr(res, "crs")
  expect_true(is.na(crs))
})


test_that(".points_as_matrix accepts matrix for multiple points", {
  p <- c(305170,	6190800)
  m <- matrix(c(p, p, p), ncol = 2, byrow = TRUE)

  res <- .points_as_matrix(m)

  expect_true(is.matrix(res))
  expect_equal(dim(res), c(nrow(m), 2))

  crs <- attr(res, "crs")
  expect_true(is.na(crs))
})


test_that(".points_as_matrix accepts an sf point", {
  p <- sf::st_point( c(305170,	6190800) )
  res <- compassutils:::.points_as_matrix(p)

  expect_true(is.matrix(res))
  expect_equal(dim(res), c(1,2))

  crs <- attr(res, "crs")
  expect_true(is.na(crs))
})

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

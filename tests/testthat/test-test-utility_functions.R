test_that("is_sf_object works", {
  coords <- c(305170,	6190800)
  p <- sf::st_point(coords)

  expect_false(is_sf_object(coords))
  expect_true(is_sf_object(p))
})

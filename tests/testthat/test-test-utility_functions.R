test_that("is_sf_object works", {
  coords <- c(305170,	6190800)

  # simple vector is not an sf object
  expect_false(is_sf_object(coords))

  # these should all return TRUE
  p <- sf::st_point(coords)
  expect_true(is_sf_object(p))

  pl <- sf::st_sfc(p, p, p, crs = 7856)
  expect_true(is_sf_object(pl))

  pdat <- sf::st_sf(index = seq_along(pl), geom = pl)
  expect_true(is_sf_object(pdat))
})

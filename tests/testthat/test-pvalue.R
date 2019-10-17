
test_that("prueba z cola derecha", {
  esperado <- 0.0010
  actual <- calculate_pvalue(statistic      = 3.08,
                             n              = 50,
                             prob_density   = "z",
                             tails          = "right")

  expect_equal(esperado, actual)
})


test_that("prueba z cola izquierda", {
  esperado <- 0.2206
  actual <- calculate_pvalue(statistic    = -0.77,
                             n            = 50,
                             prob_density = "z",
                             tails        = "left")

  expect_equal(esperado, actual)
})


test_that("prueba z dos colas", {
  esperado <- 0.1236
  actual <- calculate_pvalue(statistic    = 1.54,
                             n            = 50,
                             prob_density = "z",
                             tails        = "two")

  expect_equal(esperado, actual)
})


test_that("prueba t cola derecha", {
  esperado <- 0.0127
  actual <- calculate_pvalue(statistic    = 2.5,
                             n            = 15,
                             prob_density = "t",
                             tails        = "right")

  expect_equal(esperado, actual)
})

test_that("prueba t cola izquierda", {
  esperado <- 0.0127
  actual <- calculate_pvalue(statistic    = -2.5,
                             n            = 15,
                             prob_density = "t",
                             tails        = "left")

  expect_equal(esperado, actual)
})


test_that("prueba t dos colas", {
  esperado <- 0.0255
  actual <- calculate_pvalue(statistic    = -2.5,
                             n            = 15,
                             prob_density = "t",
                             tails        = "two")

  expect_equal(esperado, actual)
})

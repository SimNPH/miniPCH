test_that("distribution functions, classic interface are consistent", {
  times <- c(0, 1)
  hazards <- c(1, 0.5)
  x <- seq(0, 5, by=0.5)
  N <- 10
  q <- seq(0,1, by=0.2)

  my_ch <- chpch(x, times, hazards)
  my_p  <- ppch (x, times, hazards)
  my_q  <- qpch (q, times, hazards)
  r_unif <- withr::with_seed(123, runif(N))
  my_r  <- withr::with_seed(123, rpch (N, times, hazards))
  my_r_round <- withr::with_seed(123, rpch (N, times, hazards, discrete = TRUE))
  my_s  <- spch (x, times, hazards)


  expect_equal_tol(
    my_p+my_s, 1,
    "test if S and F are consistent"
  )

  expect_equal(
    my_r, qpch(r_unif, times, hazards),
    label="test if inverse transformation rng is consitent with quantiles"
  )

  expect_equal_tol(
    qpch(my_p, times, hazards), x,
    "test if quant is inverse of F"
  )

  expect_equal_tol(
    ppch(my_q, times, hazards), q,
    "test if F is inverse of quant"
  )

  expect_equal_tol(
    exp(-my_ch), my_s,
    "test if cumhaz and S are consistent"
  )

  expect_equal(
    my_r_round, floor(my_r)+1,
    label="test if discrete parameter works"
  )
})

test_that("test functions for case of exponential distribution", {
  times <- 0
  hazards <- 1

  x <- seq(0, 5, by=0.5)
  N <- 10
  q <- seq(0,0.8, by=0.2)

  my_ch <- chpch(x, times, hazards)
  my_d  <- dpch (x, times, hazards)
  my_h  <- hpch (x, times, hazards)
  my_p  <- ppch (x, times, hazards)
  my_q  <- qpch (q, times, hazards)
  my_s  <- spch (x, times, hazards)

  expect_equal_tol(
    my_ch, x,
    tol=.Machine$double.eps*2,
    "cumhaz"
  )

  expect_equal_tol(
    my_d, dexp(x, rate=hazards),
    tol=.Machine$double.eps*2,
    "density"
  )

  expect_equal_tol(
    my_h, hazards,
    tol=.Machine$double.eps*2,
    "hazards"
  )

  expect_equal_tol(
    my_p, pexp(x, rate=hazards),
    tol=.Machine$double.eps*2,
    "cdf"
  )

  expect_equal_tol(
    my_q, qexp(q, rate=hazards),
    tol=.Machine$double.eps*2,
    "quant"
  )

  expect_equal_tol(
    my_s, 1-pexp(x, rate=hazards),
    tol=.Machine$double.eps*2,
    "quant"
  )

})

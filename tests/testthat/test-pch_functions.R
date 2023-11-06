test_that("distribution functions are consistent", {
  times <- c(0, 1)
  hazards <- c(1, 0.5)
  x <- seq(0, 5, by=0.5)
  N <- 10
  q <- seq(0,1, by=0.2)

  my_ch <- chpch_fun(times, hazards)(x)
  my_p_fun <- ppch_fun (times, hazards)
  my_p  <- my_p_fun(x)
  my_q_fun <- qpch_fun (times, hazards)
  my_q  <-  my_q_fun(q)
  r_unif <- withr::with_seed(123, runif(N))
  my_r  <- withr::with_seed(123, rpch_fun(times, hazards)(N))
  my_r_round <- withr::with_seed(123, rpch_fun(times, hazards, discrete = TRUE)(N))
  my_s  <- spch_fun(times, hazards)(x)


  expect_equal_tol(
    my_p+my_s, 1,
    "test if S and F are consistent"
  )

  expect_equal(
    my_r, my_q_fun(r_unif),
    label="test if inverse transformation rng is consitent with quantiles"
  )

  expect_equal_tol(
    my_q_fun(my_p), x,
    "test if quant is inverse of F"
  )

  expect_equal_tol(
    my_p_fun(my_q), q,
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

  my_ch <- chpch_fun(times, hazards)(x)
  my_d  <- dpch_fun (times, hazards)(x)
  my_h  <- hpch_fun (times, hazards)(x)
  my_p  <- ppch_fun (times, hazards)(x)
  my_q  <- qpch_fun (times, hazards)(q)
  my_s  <- spch_fun (times, hazards)(x)

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

test_that("class constructor works", {
  times <- c(0, 1)
  hazards <- c(1, 0.5)

  my_obj <- pch_functions(times, hazards)

  expect_named(
    my_obj,
    c("d", "p", "q", "r", "h", "ch", "s", "t", "lambda", "discrete")
  )

  expect_s3_class(my_obj, "miniPCH")
})

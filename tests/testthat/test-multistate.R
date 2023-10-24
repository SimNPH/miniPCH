test_that("mstate: distribution functions, classic interface are consistent", {
  Tint <- c(1,20)
  Q <- array(NA_real_, dim=c(3,3,2))
  Q[,,1] <- matrix(
    c(
      -0.2, 0   , 0.2 ,
      0  ,-0.05, 0.05,
      0  ,    0, 0
    ), 3, 3, byrow = TRUE
  )
  Q[,,2] <- matrix(
    c(
      -0.05, 0   , 0.05 ,
      0  ,-0.05, 0.05,
      0  ,    0, 0
    ), 3, 3, byrow = TRUE
  )

  pi <- c(0.75,0.25,0)
  abs <- c(0,0,1)

  t <- seq(0, 100, by=0.1)

  my_ch <- chmstate(t, Tint, Q, pi, abs)
  my_p  <-  pmstate(t, Tint, Q, pi, abs)
  my_s  <-  smstate(t, Tint, Q, pi, abs)
  my_d  <-  dmstate(t, Tint, Q, pi, abs)


  expect_equal_tol(
    my_p+my_s, 1,
    "test if S and F are consistent"
  )

  expect_equal_tol(
    exp(-my_ch), my_s,
    "test if cumhaz and S are consistent"
  )
})

test_that("mstate: test functions for case of exponential distribution", {
  Tint <- c(0, 20)
  Q_ <- matrix(
    c(
      -0.3, 0.2, 0.1,
       0  ,-0.5, 0.5,
       0  , 0  , 0
    ), 3, 3, byrow = TRUE
  )
  Q <- array(NA_real_, dim=c(3,3,2))
  Q[,,1] <- Q_
  Q[,,2] <- Q_
  pi <- c(1,0,0)
  abs <- c(0,1,1)

  t <- seq(0, 40, by=0.1)
  rate <- -Q[1,1,1]

  my_ch <- chmstate(t, Tint, Q, pi, abs)
  my_h  <-  hmstate(t, Tint, Q, pi, abs)
  my_p  <-  pmstate(t, Tint, Q, pi, abs)
  my_s  <-  smstate(t, Tint, Q, pi, abs)
  my_d  <-  dmstate(t, Tint, Q, pi, abs)

  expect_equal_tol(
    my_ch, t*rate,
    tol=1e-7,
    "cumhaz"
  )

  expect_equal_tol(
    my_d, dexp(t, rate=rate),
    tol=.Machine$double.eps*2,
    "density"
  )

  expect_equal_tol(
    my_h, rate,
    tol=1e-7,
    "hazards"
  )

  expect_equal_tol(
    my_p, pexp(t, rate=rate),
    tol=1e-7,
    "cdf"
  )

  expect_equal_tol(
    my_s, 1-pexp(t, rate=rate),
    tol=1e-7,
    "quant"
  )

})

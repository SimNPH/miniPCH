test_that("parameter assertions work nph", {
  expect_error(check_t_lambda(c(1,1,0), c(1,1,1)))
  expect_error(check_t_lambda(c(1,0), c(1,1)))
  expect_error(check_t_lambda(c(0,1), c(1, -1)))
  expect_error(check_t_lambda(c(0,1,2), c(1,1)))
  expect_no_error(check_t_lambda(c(0,1,2), c(1,2,1)))
})

test_that("parameter assertions work multi-state", {
  Q <- array(0, dim=c(2,2,2))
  Tint <- c(0,1)
  abs <- c(0,1)
  pi <- c(1,0)

  expect_no_error(check_t_Q_pi_abs(Tint, Q, pi, abs))

  expect_error(check_t_Q_pi_abs(c(1,0), Q, pi, abs))
  expect_error(check_t_Q_pi_abs(c(0,1,2), Q, pi, abs))

  # pi, abs, Q of compatible size?
  expect_error(check_t_Q_pi_abs(Tint, Q, c(1,0,0), abs))
  expect_error(check_t_Q_pi_abs(Tint, array(0, dim=c(2,1,2)), pi, abs))
  expect_error(check_t_Q_pi_abs(Tint, Q, pi, c(1)))

  # abs an indicator vector?
  expect_error(check_t_Q_pi_abs(Tint, Q, pi, c(0.5, 0.5)))
  expect_error(check_t_Q_pi_abs(Tint, Q, pi, c(0L,2L)))
  # at least one event type of interest?
  expect_error(check_t_Q_pi_abs(Tint, Q, pi, c(0L,0L)))

  # pi a distribution?
  expect_error(check_t_Q_pi_abs(Tint, Q, c(1,1), abs))
  expect_error(check_t_Q_pi_abs(Tint, Q, c(-1,2), abs))

  # every slice of Q a Q matrix?
  Q_1 <- Q
  Q_1[1,1,1] <- -1
  expect_error(check_t_Q_pi_abs(Tint, Q_1, pi, abs))
  Q_2 <- Q
  Q_2[1,1,1] <- 1
  Q_2[1,2,1] <- -1
  expect_error(check_t_Q_pi_abs(Tint, Q_2, pi, abs))
  Q_3 <- array(0, dim=c(3,3,1))
  Q_3[,,1] <- matrix(c(
    -1, 2, -1,
     0, 0,  0,
     0, 0,  0
  ), 3,3, byrow=TRUE)
  expect_error(check_t_Q_pi_abs(0, Q_3, c(1,0,0), c(0,0,1)))

  # abs corresponds to absorbing states?
  Q_4 <- matrix(c(
    -1, 1,
     0, 0
  ), 2,2, byrow = TRUE)
  dim(Q_4) <- c(2,2,1)
  expect_error(check_t_Q_pi_abs(0, Q_4, c(0.5, 0.5), c(1,0)))
  expect_no_error(check_t_Q_pi_abs(0, Q_4, c(0.5, 0.5), c(0,1)))

  Q_5 <- array(0, dim=c(4, 4, 2))
  Q_5[1,1:2,1] <- c(-1, 1)
  Q_5[2,2:3,2] <- c(-1, 1)
  Q_5[3,3:4,2] <- c(-1, 1)
  Q_5[4,4:3,2] <- c(-1, 1)
  expect_error(check_t_Q_pi_abs(c(0,1), Q_5, c(0.25, 0.25, 0.25, 0.25), c(1,0,0,0)))
  expect_error(check_t_Q_pi_abs(c(0,1), Q_5, c(0.25, 0.25, 0.25, 0.25), c(0,1,0,0)))
  expect_error(check_t_Q_pi_abs(c(0,1), Q_5, c(0.25, 0.25, 0.25, 0.25), c(0,0,1,0)))
  expect_error(check_t_Q_pi_abs(c(0,1), Q_5, c(0.25, 0.25, 0.25, 0.25), c(0,0,0,1)))
  expect_no_error(check_t_Q_pi_abs(c(0,1), Q_5, c(0.25, 0.25, 0.25, 0.25), c(0,0,1,1)))
})

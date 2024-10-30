test_that("summary and printing work", {
  Tint <- c(0,3)
  Q <- array(
    c(
      -0.3,  0  , 0,
      0.2, -0.4, 0,
      0.1,  0.4, 0,

      -0.3,  0  , 0,
      0.2, -0.2, 0,
      0.1,  0.2, 0
    ), dim=c(3,3,2)
  )

  pi <- c(1,0,0)
  abs <- c(0,0,1)

  my_obj  <- multistate_functions(Tint, Q, pi, abs)
  my_obj2 <- pch_functions(t=c(0, 5), lambda=c(0.1, 0.2))

  summary <- summary(my_obj)
  summary2 <- summary(my_obj2)

  expect_equal(summary, list(t=Tint, Q=Q, pi=pi, abs=abs))
  expect_equal(summary2, list(t=c(0,5), lambda=c(0.1, 0.2), discrete=FALSE))

  expect_output(
    print(my_obj),
    "A miniPCH object
describing a distibution for time to absorption with 3 states and 1 absoribing states and piecewise constant transition rates on 2 time intervals:
[0, 3), [3, Inf)",
    fixed=TRUE
  )

  expect_output(
    print(my_obj2),
    "A miniPCH object
describing a survival distribution with piecewise constant hazards defined on 2 time intervals:
[0, 5), [5, Inf)
and hazards:
0.1, 0.2",
    fixed=TRUE
  )

  tmp <- capture_output(
    expect_equal(
      print(my_obj2),
      "A miniPCH object
describing a survival distribution with piecewise constant hazards defined on 2 time intervals:
[0, 5), [5, Inf)
and hazards:
0.1, 0.2"
    )
  )

  tmp <- capture_output(
    expect_invisible(
      print(my_obj2)
    )
  )
})

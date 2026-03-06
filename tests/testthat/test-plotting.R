test_that("plot method outputs the correct images", {
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

  example_plot <- function(){
    plot(my_obj)
  }

  example_plot2 <- function(){
    plot(my_obj2)
  }

  expect_error(plot(my_obj, what=c("q")))
  expect_error(plot(my_obj2, what=c("x")))

  # skip vdiffr checks on cran and gh-actions
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("multistate-plot", example_plot)
  vdiffr::expect_doppelganger("pch-plot", example_plot2)
})

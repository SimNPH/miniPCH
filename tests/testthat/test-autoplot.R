test_that("autoplot method outputs the correct images", {
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

  gg <- ggplot2::autoplot(my_obj)
  gg2 <- ggplot2::autoplot(my_obj2)

  vdiffr::expect_doppelganger("multistate-autoplot", gg)
  expect_error(ggplot2::autoplot(my_obj, what=c("q")))
  vdiffr::expect_doppelganger("pch-autoplot", gg2)
  expect_error(ggplot2::autoplot(my_obj2, what=c("x")))

  with_mocked_bindings(
    {
      expect_error(ggplot2::autoplot(my_obj))
    },
    requireNamespace = function(package, ..., quietly=FALSE) FALSE,
    .package="base"
  )
})

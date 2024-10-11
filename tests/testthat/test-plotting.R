test_that("plot method outputs the correct images", {
  example_plot <- function(){
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

    my_obj <- multistate_functions(Tint, Q, pi, abs)
    plot.miniPCH(my_obj)
  }

  vdiffr::expect_doppelganger("multistate-plot", example_plot)
})

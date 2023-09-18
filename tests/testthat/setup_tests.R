

expect_equal_tol <- function(x, y, tol=.Machine$double.eps * 4, info=NULL, label=NULL){
  expect_true(
    all(abs(x - y) < .Machine$double.eps*4),
    info=info,
    label=label
  )
}

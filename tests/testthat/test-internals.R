test_that("parameter assertions work", {
  expect_error(check_t_lambda(c(1,1,0), c(1,1,1)))
  expect_error(check_t_lambda(c(1,0), c(1,1)))
  expect_error(check_t_lambda(c(0,1), c(1, -1)))
  expect_error(check_t_lambda(c(0,1,2), c(1,1)))
  expect_no_error(check_t_lambda(c(0,1,2), c(1,2,1)))
})

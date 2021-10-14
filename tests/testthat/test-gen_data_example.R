test_that("lengths and dimensions", {
  n_1 = 3000
  n_2 = 1000
  k_1 = 6
  k_2 = 3
  p_1 = 5
  p_2 = 4

  data_one = gen_data_example(n=n_1, k = k_1, p = p_1)
  data_two = gen_data_example(n=n_2, k = k_2, p = p_2)
  #check the dimension of covariates
  expect_equal(dim(data_one$x), c(n_1, p_1))
  expect_equal(dim(data_two$x), c(n_2, p_2))

  #check the dimension of response
  expect_length(data_one$y, n_1)
  expect_length(data_two$y, n_2)

  #check the number of categoies
  expect_equal(data_one$y %>% unique() %>% length(), k_1)
  expect_equal(data_two$y %>% unique() %>% length(), k_2)

  #check gamma param
  expect_length(data_one$gamma, k_1 + 1)
  expect_length(data_two$gamma, k_2 + 1)
  expect_true(is.infinite(data_one$gamma[1]))
  expect_true(is.infinite(data_one$gamma[k_1 + 1]))
  expect_true(is.infinite(data_two$gamma[1]))
  expect_true(is.infinite(data_two$gamma[k_2 + 1]))
})


test_that("return_types", {
  #check if is list
  data = gen_data_example(n=3000, k = 6, p = 5)
  expect_true(data %>% is.list())
  expect_true(data$x %>% is.matrix())
  expect_true(data$y %>% is.numeric())
  expect_true(data$deltas %>% is.numeric())
  expect_true(data$gamma %>% is.numeric())
})

test_that("args_errors", {
  expect_error(gen_data_example(n=10.1, k = 6, p = 5, q = 0.4))
  expect_error(gen_data_example(n=100, k = 6, p = 200, q = 0.4))
  expect_error(gen_data_example(n=100, k = 6, p = 200, q = 1))
  expect_error(gen_data_example(n=1000, k = 6, p = 200, q = 2.2))
  expect_error(gen_data_example(n=1000, k = 1, p = 200, q = 0.4))
  expect_error(gen_data_example(n=1000, k = 4.5, p = 200, q = 0.4))
  expect_error(gen_data_example(n=1000, k = 6, p = -2, q = 0.4))
  expect_error(gen_data_example(n=-1000, k = 6, p = 10, q = 0.4))

})





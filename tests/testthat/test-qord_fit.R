test_that("checking input", {
  data = gen_data_example(n=1000, k = 3, seed = 1, p = 6, q = 0.5)
  #q not in the (0, 1 ) interval
  expect_error(qord_fit(y ~ ., q = -1, data = data$example_df, iter = 100))
  expect_error(qord_fit(y ~ ., q = 0, data = data$example_df, iter = 100))
  expect_error(qord_fit(y ~ ., q = 2, data = data$example_df, iter = 100))
  #prior scales less than 0
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = 0, data = data$example_df, iter = 100))
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = -1, data = data$example_df, iter = 100))

  expect_error(qord_fit(y ~ ., q = 0.5, beta_scale = 0, data = data$example_df, iter = 100))
  expect_error(qord_fit(y ~ ., q = 0.5, beta_scale = -1, data = data$example_df, iter = 100))

  #iter and warmup
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = 0, data = data$example_df, iter = 0))
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = -1, data = data$example_df, iter = 100,
                        warmup = 200))

  #check response variable
  df = data$example_df
  df['y', 1:3] = NA
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = 0, data = df, iter = 100))

  df = data$example_df
  df['y', 1:3] = 1.1
  expect_error(qord_fit(y ~ ., q = 0.5, delta_scale = 0, data = df, iter = 100))

})

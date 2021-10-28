test_that("returns", {
  data = gen_data_example(n=1000, k = 3, seed = 1, p = 6, q = 0.5)
  fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 100)$stan_fit

  out = new_qordstan(
    fit,
    formula = y ~ .,
    x = data$x,
    y = data$y,
    q = 0.5,
    beta_scale = 100,
    delta_scale = 0.25
  )

  expect_s3_class(out, class = 'qordstan')

})

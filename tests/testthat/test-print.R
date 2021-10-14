test_that("print returns", {
  data = gen_data_example(n=100, k = 4, p = 2)
  fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 50, warmup = 5)
  expect_null(print(fit))
  expect_null(print(summary(fit)))
})

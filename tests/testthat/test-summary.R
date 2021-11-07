test_that("error inputs", {

  k = 6
  p = 2

  data = gen_data_example(n=300, k = k, p = p)
  fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 20,
                 verbose = F, show_messages = F)

  expect_error(summary(fit, cred_mass = 2))
  expect_error(summary(fot, cred_mass = 0))
})

test_that("summary dimensions", {
  k = 6
  p = 2

  data = gen_data_example(n=300, k = k, p = p)
  fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 20,
                 verbose = F, show_messages = F)
  #check if is list
  sm = summary(fit)
  sm_dim = sm$summary_table %>% dim()
  expect_equal(sm_dim[2], 4)
  expect_equal(sm_dim[1], p + (k - 2))
  expect_equal(length(sm$beta_mean), p)
  expect_equal(length(sm$gamma_mean), k-2)
  expect_true(is.numeric(sm$beta_mean))
  expect_true(is.numeric(sm$gamma_mean))
  expect_equal(class(sm$waic), c("waic", "loo"))
})


test_that("Dimensions", {
  k = 6
  p = 2

  data = gen_data_example(n=300, k = k, p = p)
  new_data = gen_data_example(n=100, k=k, p = p)

  fit = qord_fit(y ~ ., q = 0.5, data = data$example_df, iter = 20,
                 verbose = F, show_messages = F)


  pred_cat = predict(fit, type = 'cat')
  pred_z = predict(fit, type = 'z')

  pred_new_cat = predict(fit, type = 'cat', new_data = new_data$example_df)
  pred_new_z = predict(fit, type = 'z', new_data = new_data$example_df)

  expect_equal(dim(pred_cat)[2], 300)
  expect_equal(dim(pred_z)[2], 300)
  expect_equal(dim(pred_new_cat)[2], 100)
  expect_equal(dim(pred_new_z)[2], 100)

  expect_equal(dim(pred_cat)[1], 10)
  expect_equal(dim(pred_z)[1], 10)
  expect_equal(dim(pred_new_cat)[1], 10)
  expect_equal(dim(pred_new_z)[1], 10)
})

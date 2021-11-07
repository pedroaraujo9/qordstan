test_that("number of categories", {
  #z values
  z_1 = c(-10, 0.5, 1.5, 2.5, 3.5)
  z_2 = c(-10, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
  #cutpoiints
  gamma_1 = c(1, 2, 3)
  gamma_2 = c(1, 2, 3, 4, 5, 6)
  #cats
  cat_1 = .z_to_cat(z_1, gamma_1)
  cat_1
  cat_2 = .z_to_cat(z_2, gamma_2)
  cat_2
  #tests
  expect_equal(unique(cat_1) %>% length(), length(gamma_1) + 2)
  expect_equal(unique(cat_2) %>% length(), length(gamma_2) + 2)
  expect_error(.z_to_cat(z_1, gamma = c(-1)))

})


test_that("lenghts", {
  x = rnorm(1000)
  res = posterior_resume(x)
  expect_length(res, 4)
  expect_named(res, c("mean", "std", "HDI LI", "HDI UI"))
})

test_that("main function works", {

  test_data <- fastFeatures:::test_data_1

  results <- fastFeatures::cVIP(
    df = test_data,
    target_column = "y",
    feature_columns = c("x1", "x2", "x3"),
    column_proportion=0.75,
    record_proportion=0.99,
    n_iterations=10,
    l1_lambda=3,
    glmnet_family="gaussian"
  )
  expect_gte(results$"Conditional Variable Inclusion Probability"[2], 0)
  expect_lte(results$"Conditional Variable Inclusion Probability"[2], 1)
  expect_gte(results$"Conditional Variable Inclusion Probability"[3], 0)
  expect_gte(results$"Conditional Variable Inclusion Probability"[4], 0)
  expect_lte(results$"Conditional Variable Inclusion Probability"[3], 1)
  expect_lte(results$"Conditional Variable Inclusion Probability"[4], 1)
  expect_s3_class(results, "data.frame")

  set.seed(NULL)

})


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

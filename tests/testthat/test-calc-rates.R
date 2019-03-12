context("test-calc-rates")

test_that("true positive and false positive rates are calculated correctly", {
  expect_equal(
    calc_tpr(
      pred_values = c(1:4),
      pos_pred = c(3, 4),
      npos = 2
    ), c(1, 1, 1, 0.5))
})

context("test-calc-auc")

test_that("area under the curve is correct", {

  # ROC curve is a diagonal (AUC = 0.5)
  df <- data.frame(
    x = seq(0, 1, 0.2),
    y = seq(0, 1, 0.2)
  )

  expect_equal(calc_auc(x = df$x, y = df$y), 0.5)

  # ROC curve is a right angle (AUC = 1)
  df <- data.frame(
    x = c(0, 0, 0, 0.5, 1),
    y = c(0, 0.5, 1, 1, 1)
  )

  expect_equal(calc_auc(x = df$x, y = df$y), 1)

  # ROC curve is an inverted right angle (AUC = 0)
  df <- data.frame(
    x = c(0, 0.5, 1, 1, 1),
    y = c(0, 0, 0, 0.5, 1)
  )

  expect_equal(calc_auc(x = df$x, y = df$y), 0)

  # precision-recall is a right angle (AUC = 1)
  df <- data.frame(
    x = c(0, 1, 1),
    y = c(1, 1, 0.5)
  )

  expect_equal(calc_auc(x = df$x, y = df$y), 1)

  # precision-recall is a line in the middle (AUC = 0.5)
  df <- data.frame(
    x = c(0, 0, 1),
    y = c(1, 0.5, 0.5)
  )

  expect_equal(calc_auc(x = df$x, y = df$y), 0.5)

})

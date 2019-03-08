context("test-make-roc")

test_that("ROC curve values are correct", {

  # predictor perfectly separate the two outcomes
  df <- data.frame(
    predictor = c(0, 0, 0, 10, 10, 10),
    known_class = c(0, 0, 0, 1, 1, 1) # 1 is positive, 0 is negative
  )

  # ROC should be a right angle
  expect_equal(
    df %>% make_roc(predictor, known_class),
    df %>%
      add_row(
        predictor = NA,
        known_class = NA,
        .before = 1) %>%
      cbind(
        tpr = c(0, 1, 1, 1, 1, 1, 1),
        fpr = c(0, 1, 1, 1, 0, 0, 0)
      ) %>%
      arrange(fpr, tpr)
  )

  # predictor is a random classifier of two outcomes
  df <- data.frame(
    predictor = c(0, 0, 0, 0, 0, 0),
    known_class = c(0, 0, 0, 1, 1, 1) # 1 is positive, 0 is negative
  )

  # ROC should be a diagonal
  expect_equal(
    df %>% make_roc(predictor, known_class),
    df %>%
      add_row(
        predictor = NA,
        known_class = NA,
        .before = 1) %>%
      cbind(
        tpr = c(0, 1, 1, 1, 1, 1, 1),
        fpr = c(0, 1, 1, 1, 1, 1, 1)
      ) %>%
      arrange(fpr, tpr)
  )

  # predictor contains infinity
  df <- data.frame(
    predictor = c(-Inf, 0, 0, 10, 10, 10),
    known_class = c(0, 0, 0, 1, 1, 1) # 1 is positive, 0 is negative
  )

  # ROC should be a right angle
  expect_equal(
    df %>% make_roc(predictor, known_class),
    df %>%
      add_row(
        predictor = NA,
        known_class = NA,
        .before = 1) %>%
      cbind(
        tpr = c(0, 1, 1, 1, 1, 1, 1),
        fpr = c(0, 1, 2/3, 2/3, 0, 0, 0)
      ) %>%
      arrange(fpr, tpr)
  )

})

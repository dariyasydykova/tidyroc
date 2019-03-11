context("test-make-pr")

test_that("precision-recall curve is plotted correctly", {

  # predictor perfectly separates the two outcomes
  df <- data.frame(
    predictor = c(0, 0, 0, 10, 10, 10),
    known_class = c(0, 0, 0, 1, 1, 1) # 1 is positive, 0 is negative
  )

  # precision-recall should be a right angle
  expect_equal(
    df %>% make_pr(predictor, known_class),
    df %>%
      add_row(
        predictor = NA,
        known_class = NA,
        .before = 1) %>%
      cbind(
        recall = c(0, 1, 1, 1, 1, 1, 1),
        precision = c(1, 3/6, 3/6, 3/6, 3/3, 3/3, 3/3)
      ) %>%
      arrange(recall, desc(precision))
  )


  # predictor is a random classifier of two outcomes
  df <- data.frame(
    predictor = c(0, 0, 0, 0, 0, 0),
    known_class = c(0, 0, 0, 1, 1, 1) # 1 is positive, 0 is negative
  )

  # precision-recall should be a straing line at precision = 0.5
  expect_equal(
    df %>% make_pr(predictor, known_class),
    df %>%
      add_row(
        predictor = NA,
        known_class = NA,
        .before = 1) %>%
      cbind(
        recall = c(0, 1, 1, 1, 1, 1, 1),
        precision = c(1, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2)
      ) %>%
      arrange(recall, desc(precision))
  )

})

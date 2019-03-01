#' Receiver operating characteristic curve (ROC)
#'
#' this function calculates true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' calc_ROC(glm.out$fitted.values, lr_data$Species, model.name = "model1", data = lr_data)

calc_ROC <- function(probabilities, known.truth, model.name = NULL, data = NULL) {

  # convert categorical (`known_truth`) to numeric to perform calculations on it
  # positives = 1, negatives = 0
  outcome <- as.numeric(factor(known.truth)) - 1 # use factors to match glm() output

  # count total positives and total negatives to calculate true positive and false positive rates
  pos <- sum(outcome) # total known positives
  neg <- sum(1 - outcome) # total known negatives

  # get probability values for each positive and negative outcome
  pos_probs <- outcome * probabilities # probabilities for known positives
  neg_probs <- (1 - outcome) * probabilities # probabilities for known negatives

  # calculate true positive rate
  true_pos <- sapply(
    probabilities,
    function(x) sum(pos_probs >= x) / pos
  )

  # calculate false positive rate
  false_pos <- sapply(
    probabilities,
    function(x) sum(neg_probs >= x) / neg
  )

  # if given, add model name to the results
  if (is.null(model.name)) {
    result <- data.frame(true_pos, false_pos) %>%
      add_row(true_pos = 0, false_pos = 0) %>% # add 0, 0 to extend an ROC curve
      arrange(false_pos, true_pos)
  } else {
    result <-
      data.frame(true_pos, false_pos, model_name = model.name) %>%
      add_row(true_pos = 0, false_pos = 0, model_name = model.name) %>% # add 0, 0 to extend an ROC curve
      arrange(false_pos, true_pos)
  }

  # if given, add original data frame to the results
  if (!is.null(data)) {
    result <-
      data %>%
      add_row(.before = 1) %>%
      data.frame(result)
  }

  result
}

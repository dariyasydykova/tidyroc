#' Receiver operating characteristic (ROC) curve
#'
#' this function calculate true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' roc()

roc <- function(data, predictor, positive) {

  # use tidy eval
  predictor <- rlang::enquo(predictor)
  positive <- rlang::enquo(positive)

  pred_values <- rlang::eval_tidy(predictor, data)
  pos_values <- rlang::eval_tidy(positive, data)

  # convert known outcomes to numeric to perform calculations on it
  # positives = 1, negatives = 0
  pos_values <- as.numeric(factor(pos_values)) - 1 # use factors to match glm() output

  # count total positives and total negatives to calculate true positive and false positive rates
  pos <- sum(pos_values) # total known positives
  neg <- sum(1 - pos_values) # total known negatives

  # get predictor values for positive and negative outcomes to figure out whether it is a true positive or a false positive
  pos_pred <- pred_values[pos_values == 1] # predictors for known positives
  neg_pred <- pred_values[pos_values == 0] # predictors for known negatives

  # calculate the true positive rate
  true_pos <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / pos
  )

  # calculate the false positive rate
  false_pos <- sapply(
    pred_values,
    function(x) sum(neg_pred >= x) / neg
  )

  # add true positive rate and false positive rate to the data-frame `data`
  # add true positive = 0 and false positive = 0 to make sure an ROC curve always starts at 0, 0
  data %>%
    mutate(true_pos, false_pos) %>%
    add_row(true_pos = 0, false_pos = 0) %>%
    arrange(false_pos, true_pos) # order output by false positive and then true positive rate to plot the ROC curve correctly
}

#' Precision-recall curve
#'
#' this function calculates precision and recall to plot a precision-recall curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples

prec_recall <- function(data, predictor, positive) {

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

  # get predictor values for positive and negative outcomes to figure out whether it is a true positive or a false positive
  pos_pred <- pred_values[pos_values == 1] # predictors for known positives
  neg_pred <- pred_values[pos_values == 0] # predictors for known negatives

  # calculate the true positive rate
  recall <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / pos
  )

  # calculate the positive predictive value
  precision <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / (sum(pos_pred >= x) + sum(neg_pred >= x))
  )

  # add recall and precision to the data-frame `data`
  data %>%
    mutate(recall, precision) %>%
    arrange(recall, desc(precision)) # order output by recall (ascending order) and then precision (descending order) to plot the precision recall curve correctly
}

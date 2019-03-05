#'
#'
#' this function measures different binary classification values
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' roc()

calc_tpr <- function(pred_values, pos_pred, pos){
  # calculate the true positive rate
  true_pos_rate <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / pos
  )

  true_pos_rate
}

calc_tnr <- function(pred_values, neg_pred, neg){
  # calculate the true positive rate
  true_neg_rate <- sapply(
    pred_values,
    function(x) sum(neg_pred < x) / neg
  )

  true_neg_rate
}

calc_fpr <-function(pred_values, neg_pred, neg){
  # calculate the false positive rate
  false_pos_rate <- sapply(
    pred_values,
    function(x) sum(neg_pred >= x) / neg
  )

  false_pos_rate
}

calc_fnr <-function(pred_values, pos_pred, pos){
  # calculate the false positive rate
  false_neg_rate <- sapply(
    pred_values,
    function(x) sum(pos_pred < x) / pos
  )

  false_neg_rate
}

calc_ppv <- function(pred_values, pos_pred, neg_pred){
  pos_pred_value <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / (sum(pos_pred >= x) + sum(neg_pred >= x))
  )

  pos_pred_value
}

measure_perf_ungrouped <- function(data, key, predictor, true_class) {

  # use tidy eval
  predictor <- rlang::enquo(predictor)
  true_class <- rlang::enquo(true_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  pos_values <- rlang::eval_tidy(true_class, data)

  # convert known outcomes to numeric to perform calculations on it
  # positives = 1, negatives = 0
  pos_values <- as.numeric(factor(pos_values)) - 1 # use factors to match glm() output

  # count total positives and total negatives to calculate true positive and false positive rates
  pos <- sum(pos_values) # total known positives
  neg <- sum(1 - pos_values) # total known negatives

  # get predictor values for positive and negative outcomes to figure out whether it is a true positive or a false positive
  pos_pred <- pred_values[pos_values == 1] # predictors for known positives
  neg_pred <- pred_values[pos_values == 0] # predictors for known negatives

  # add true positive rate and false positive rate to the data-frame `data`
  # add true positive = 0 and false positive = 0 to make sure an ROC curve always starts at 0, 0
  data %>%
    dplyr::mutate(
      tpr = calc_tpr(),
      tnr = calc_tnr(),
      fpr = calc_fpr(),
      fnr = calc_fnr(),
      ppv = calc_ppv()
    )
}

measure_perf <- function(data, ...) {
  group_map(data, measure_perf_ungrouped, ...)
}

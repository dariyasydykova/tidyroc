#'
#'
#' this function measures different binary classification values like true positive rate, false positive rate, true negative rate, false negative rate, and positive predictive value
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' roc()

# this functions calculates the true positive rate (TPR)
calc_tpr <- function(pred_values, pos_pred, pos){
  true_pos_rate <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / pos
  )

  true_pos_rate
}

# this functions calculates the false positive rate (FPR)
calc_fpr <-function(pred_values, neg_pred, neg){
  false_pos_rate <- sapply(
    pred_values,
    function(x) sum(neg_pred >= x) / neg
  )

  false_pos_rate
}

# this functions calculates the true negative rate (TNR)
calc_tnr <- function(pred_values, neg_pred, neg){
  true_neg_rate <- sapply(
    pred_values,
    function(x) sum(neg_pred < x) / neg
  )

  true_neg_rate
}

# this functions calculates the false negative rate (FNR)
calc_fnr <-function(pred_values, pos_pred, pos){
  false_neg_rate <- sapply(
    pred_values,
    function(x) sum(pos_pred < x) / pos
  )

  false_neg_rate
}

# this functions calculates the positive predictive value (PPV)
calc_ppv <- function(pred_values, pos_pred, neg_pred){
  pos_pred_value <- sapply(
    pred_values,
    function(x) sum(pos_pred >= x) / (sum(pos_pred >= x) + sum(neg_pred >= x))
  )

  pos_pred_value
}

# this function uses predictor values and known classification to measure performace of a binary classification model
# this function works on data-frames that are not grouped
measure_perf_ungrouped <- function(data, key, predictor, known_class) {

  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # convert known outcomes to numeric to perform calculations on it
  # positives = 1, negatives = 0
  pos_values <- as.numeric(factor(known_values)) - 1 # use factors to match glm() output

  # count total positives and total negatives to calculate true positive and false positive rates
  pos <- sum(pos_values) # total known positives
  neg <- sum(1 - pos_values) # total known negatives

  # get predictor values for positive and negative outcomes to figure out whether it is a true positive or a false positive
  pos_pred <- pred_values[pos_values == 1] # predictors for known positives
  neg_pred <- pred_values[pos_values == 0] # predictors for known negatives

  # adds true positive rate and false positive rate to the data-frame `data`
  data %>%
    dplyr::mutate(
      tpr = calc_tpr(pred_values, pos_pred, pos),
      fpr = calc_fpr(pred_values, neg_pred, neg),
      tnr = calc_tnr(pred_values, neg_pred, neg),
      fnr = calc_fnr(pred_values, pos_pred, pos),
      ppv = calc_ppv(pred_values, pos_pred, neg_pred)
    )
}

# this function uses predictor values and known classification to measure performace of a binary classification model
# this function works on grouped data-frames
measure_perf <- function(data, ...) {
  group_map(data, measure_perf_ungrouped, ...)
}

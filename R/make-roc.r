#' Receiver operating characteristic (ROC) curve
#'
#' this function calculates true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' roc()

make_roc <- function(data, predictor, known_class) {
  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # get binary classification values
  df <- measure_perf(data, pred_values, known_values)

  # add true positive rate and false positive rate to the data-frame `data`
  # add true positive = 0 and false positive = 0 to make sure an ROC curve always starts at 0, 0
  df %>%
    dplyr::select(tpr, fpr) %>%
    dplyr::add_row(true_pos = 0, false_pos = 0) %>%
    dplyr::arrange(fpr, tpr) # order output by false positive and then true positive rate to plot an ROC curve correctly
}

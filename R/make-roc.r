#' Receiver operating characteristic (ROC) curve
#'
#' this function calculates true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#' roc()

# this function calls `measure_perf()` to get true positive and false positive rates
# this function works on ungrouped data-frames
make_roc_ungrouped <- function(data, key, predictor, known_class) {
  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # get binary classification values
  df <- measure_perf(data = data, predictor = pred_values, known_class = known_values)

  # add true positive rate and false positive rate to the data-frame `data`
  df %>%
    dplyr::select(-c(tnr, fnr, ppv)) %>% # remove extra columns to keep data-frames simpler
    dplyr::add_row(tpr = 0, fpr = 0) %>% # add true positive = 0 and false positive = 0 to make sure an ROC curve is always anchored at 0, 0
    dplyr::arrange(fpr, tpr) # order output by false positive and then true positive rate to plot an ROC curve correctly
}

# this function calls `measure_perf()` to get true positive and false positive rates
# this function works on grouped data-frames
make_roc <- function(data, ...) {
  group_map(data, make_roc_ungrouped, ...)
}

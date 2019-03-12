#' Receiver operating characteristic (ROC) curve
#'
#' this function calculates true positive rate and false positive rate to make an ROC curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples
#'
#'

# this function calls `measure_perf()` to get true positive rate, false positive rate, true negative rate, false negative rate
# this function works on grouped data-frames
make_conf_matrix <- function(data, ...) {
  group_map(data, make_conf_matrix_ungrouped, ...)
}

# this function calls `measure_perf()` to get true positive rate, false positive rate, true negative rate, false negative rate
# this function works on ungrouped data-frames
make_conf_matrix_ungrouped <- function(data, key, cutoff, predictor, known_class) {
  # use tidy eval
  predictor <- rlang::enquo(predictor)
  known_class <- rlang::enquo(known_class)

  pred_values <- rlang::eval_tidy(predictor, data)
  known_values <- rlang::eval_tidy(known_class, data)

  # get binary classification values
  df <- measure_perf(data = data, predictor = pred_values, known_class = known_values)

  # get predicted positives
  pos_rates <-
    df %>%
    dplyr::filter(.fitted >= cutoff) %>%
    tail(n = 1) %>%
    dplyr::select(tpr, fpr) # remove extra columns to keep data-frames simpler

  # get predicted negatives
  neg_rates <-
    df %>%
    dplyr::filter(.fitted < cutoff) %>%
    head(n = 1) %>%
    dplyr::select(tnr, fnr) # remove extra columns to keep data-frames simpler

  rates <-
    cbind(pos_rates, neg_rates) %>%
    gather(values, rates, tpr:fnr)
}

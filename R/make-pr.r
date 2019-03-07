#' Precision-recall curve
#'
#' this function calculates precision and recall to plot a precision-recall curve
#' @param probabilities
#'
#' @keywords
#' @export
#' @examples

calc_prec_recall <- function(data, predictor, positive) {
  # get binary classification values
  df <- measure_perf(data, predictor, known_class)

  # add recall and precision to the data-frame `data`
  df %>%
    dplyr::select(-c(tnr, fpr, fnr), recall = tpr, precision = ppv) %>%
    dplyr::arrange(recall, desc(precision)) # order output by recall (ascending order) and then precision (descending order) to plot the precision-recall curve correctly
}

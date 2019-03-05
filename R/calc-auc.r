#' Area under an ROC curve (AUC)
#'
#' this function calculates the area under an ROC curve
#' @param
#'
#' @keywords
#' @export
#' @examples
#'

calc_auc <- function(x, y) {
  # use tidy eval
  x <- rlang::enquo(x)
  false_pos <- rlang::enquo(false_pos)

  true_pos_values <- rlang::eval_tidy(true_pos, data)
  false_pos_values <- rlang::eval_tidy(false_pos, data)

  width = false_pos_values - lag(false_pos_values)
  height1 = true_pos_values
  height2 = true_pos_values - lag(true_pos_values)
  area = width*(height1 + height2 * 0.5)

  sum(area)
}

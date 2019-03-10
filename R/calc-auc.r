#' Area under a curve (AUC)
#'
#' this function calculates the area under an ROC curve or the area under a precision-recall curve
#' @param
#'
#' @keywords
#' @export
#' @examples
#'

calc_auc <- function(x, y) {
  # get the width between each point to get the width of the rectangle
  width = abs(lead(x) - x)

  # get the height of the rectanlge
  height1 = y

  # get the height of the triangle
  height2 = abs(lead(y) - y)

  # calculate the area of the rectanlge and the triangle
  area = width*(height1 + height2 * 0.5)

    # sum all the rectangles and triangles
  sum(area, na.rm = TRUE)
}

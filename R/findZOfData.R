#' Create z from vector
#'
#' @param x quantitative vector
#'
#' @importFrom stats sd
#'
#' @return a list containing z and x
#' @export
#'
#' @examples
#' findzOfData(1:4)
findzOfData <- function(x){#x is a vector
  z <- (x - mean(x)) / sd(x)
  list(z=z,x=x)
}

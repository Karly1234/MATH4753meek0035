#' Confidence Interval
#'
#' @param x the singular dataset
#'
#'@importFrom stats t.test
#' @return a confidence interval for the dataset
#' @export
#'
#' @examples
#' myci(c(1,2,3,4,5,6,7))
myci = function(x){
  t.test(x)$conf
}

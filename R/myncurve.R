#' Find the area of a curve
#'
#' @param mu the mean of the function
#' @param sigma the standard deviation of the function
#' @param a the max x-coordinate of the area
#'
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#'
#'
#' @return area to terminal and graph of area under the curve
#' @export
#'
#' @examples
#' myncurve(10,5,6)
myncurve = function(mu, sigma,a){
  x<- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))

  xcurve = seq(mu-3*sigma,a, length = 10000)
  ycurve = dnorm(xcurve, mu, sigma)

  polygon(x = c(mu-3*sigma,xcurve, a), y = c(0,ycurve, 0), col = "pink")

  prob = pnorm(a, mu, sigma)
  prob = round(prob,4)

  print(prob)
}

#' Find optimal number of tickets to sell
#'
#' @param N Number of available seats
#' @param gamma Probability the plane will truly be overbooked
#' @param p probability that the person will show
#'
#' @importFrom stats uniroot
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom graphics lines
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics abline
#' @return 2 graphs continuous and discrete for the optimal number of tickets sold, with a list of all the variables
#' @export
#'
#' @examples
#'ntickets(200, .02, .95)
ntickets <- function(N, gamma, p) {

  # Create a sequence of values for n
  n <- seq(N, N + 20, by = 1)

  # Calculate the objective function for the discrete case
  for(i in n){
    obj_func_discrete <- abs(pbinom(N,n,p) -1 + gamma)
  }

  #Find lowest point in the graph
  ind<- which.min(obj_func_discrete)
  nd <- n[ind]

  # Plot the objective function discrete
  par(cex.main = 0.8)
  plot(n, obj_func_discrete, col = "blue", xlab = "n", ylab =
  "Objective",main=substitute(paste("Objective vs n to find the optimal
  tickets sold (",nd, ") gamma = ", gamma, " N = ",N, " discrete")))

  lines(n,obj_func_discrete, type = "l", col = "blue")
  points(n, obj_func_discrete, pch = 19, col = "blue")


  abline(h = 0 , col = "red", lty = 1)
  abline(v = nd, col = "red", lty = 1)

  # Calculate the objective function for the continuous case
  for(i in n){
    obj_func_continuous <- abs(pnorm(N +0.5, n*p, sqrt(n * p * (1 - p))) - 1 + gamma)
  }
  #Used to in the function uniroot to find n
  con <- function(n){
    return (pnorm(N +0.5, n*p, sqrt(n * p * (1 - p))) - 1 + gamma)
  }

  #Find the nc using uniroot
  nc <- uniroot(con, c(N, N + 20))
  nc2 <- nc$root

  # Plot the objective function
  par(cex.main = 0.8)
  plot(n, obj_func_continuous,type = "l",col = "black",  xlab = "n", ylab =   "Objective", main = substitute(paste("Objective vs n to find the optimal    tickets sold (",nc2, ") gamma = ", gamma, " N = ",N, " continuous")))
  abline(h = 0 , col = "blue", lty = 1)
  abline(v = nc, col = "blue", lty = 1)

  # Create a named list with the results
  result_list <- list(nd = nd, nc = nc$root, N = N, p = p, gamma = gamma)

  # Print the named list
  print(result_list)
}





#' Create a barplot of random samples
#'
#' @param iter iterations through the sample
#' @param n sample size
#' @param p probability of Success
#'
#'@importFrom stats sd
#'@importFrom graphics barplot
#'@importFrom grDevices rainbow
#'
#' @return barplot with table
#' @export
#'
#' @examples
#' mybinbarplot(500, 10, 1)
mybinbarplot=function(iter, n, p){

  # make a matrix to hold the samples labeled NA
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  #Make a vector to hold the number of successes in each sample
  succ=c()

  #loops through the number of iterations specified
  for(i in 1:iter){

    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0), n, replace=TRUE, prob=c(p,1-p))

    #Calculate a statistic from the sample, below is the sum
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))

  #Make a barplot with table of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

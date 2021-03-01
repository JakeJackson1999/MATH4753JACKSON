#' @title a barplot
#'
#' @param iter number of itterations
#' @param n samples size
#' @param p
#'
#' @return a barplot
#' @export
#'
#' @examples
#' \dontrun{mybin()}
mybin=function(iter=100, n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ",")
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", sub = lab, xlab="Number of successes")
  succ.tab/iter
}

#' @title a curve
#'
#' @param mu mean
#' @param sigma sd
#' @param a limit
#'
#' @return a curve that is shaded
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5, a=6)}
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(-5,a,length=1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(-5,xcurve,a),c(0,ycurve,0),col="black")
  pnorm(a,mu,sigma)
}

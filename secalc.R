secalc <- function(x) {
  origse <- summary(x)$coefficients[1,2]
  n <- length(x$residuals)
  origv <- (origse*sqrt(n))^2
  k <-  length(x$coefficients)-1
  newv <- (n/(n+k))^2*origv
  newse <- sqrt(newv)/sqrt(n)
  return(newse)
}
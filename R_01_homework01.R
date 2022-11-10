#R - Homework 01
#The Babylonian square-root algorithm

# x - initial value
# prec - the expected precision of the approximation
# est - first estimate 
# v = x / est
# new est = (v + est) / 2 - new estimate
# Once the difference between estimate and new estimate is lower than prec new_est would be estimatet root of x

root <- function(x, est = 1, prec = 0.001){
  v = x/est
  new_est = (v + est)/2
  
  while (abs(new_est - est) > prec){
    est <- new_est
    v = x/est
    new_est = (v + est)/2
  }
  return(new_est)
}

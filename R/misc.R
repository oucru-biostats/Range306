`%|%` <- function(lhs, rhs){
  ifelse(falsified(lhs), rhs, lhs)
}

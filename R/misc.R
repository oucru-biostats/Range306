`%|%` <- function(lhs, rhs){
  ifelse(is_falsified(lhs), rhs, lhs)
}

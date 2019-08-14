#' Check if an object is of a class
#' @param x an object
#' @param class a vector of class. A value of FALSE, TRUE, and NULL will trigger respective vectorized is_ functions.
#' @return a logical vector
#' @export
is_<-function(x, class){
  if (is.null(class)) return(is_null(x))
  if (isTRUE(class)) return(is_true(x))
  if (isFALSE(class)) return(is_false(x))
  if (is.na(class)) return(is_na(x))
  !as.logical(length(setdiff(class, class(x))))
}

#' Element-wise wrapper for is.null
#' @param x a vector or object
#' @return a logical vector
#' @export
is_null <- function(x){
  if(!is.null(x)) sapply(x, is.null) else TRUE
}

#' Element-wise wrapper for isTRUE
#' @param x a vector or object
#' @return a logical vector
#' @export
is_true <- function(x){
  if(is.null(x)) return(FALSE)
  sapply(x, isTRUE)
}

#' Element-wise wrapper for isFALSE
#' @param x a vector or object
#' @return a logical vector
#' @export
is_false <- function(x){
  if(is.null(x)) return(FALSE)
  sapply(x, isFALSE)
}

#' Element-wise wrapper for is.character
#' @param x a vector or object
#' @return a logical vector
#' @export
is_character <- function(x){
  sapply(x, function(.x) isTRUE(is.character(.x)))
}

#' Element-wise wrapper for is.na
#' @param x a vector or object
#' @return a logical vector
#' @export
is_na <- function(x){
  if(is.null(x)) return(FALSE)
  sapply(x, function(.x) isTRUE(is.na(.x)))
}

#' Check if some objects don't return a legible value
#' @param x a vector or object
#' @return a logical vector
#' @export
is_falsified <- is.falsified <- function(x){
  if (is.null(x)) return(TRUE)
  sapply(x, function(.x) {
    any(is.na(.x), is.null(.x), is_false(.x))
  })
}


#' Set class
#'
#' @description An R6 object for discrete value set
#' @usage
#' Set
#'  $new(..., .list)
#'  $print()
#'  $include(x)
#'  $raw
#'  $val
#'
#' @param ... a spreaded list of values.
#' @param .list an unspreaded list of values.
#' @param x a vector of objects to check if they are included in the set
#'
#' @section Class Methods:
#' $new: the class constructor
#'
#' $print: the print method
#'
#' $include: check if an object is included in the set
#'
#' $raw: print a beautified string describing the set
#'
#' $val: return a conventional list
#'
#' @return
#' $new: an object of R6 class "Set"
#'
#' $include: a logical vector of length == length(x)
#'
#' $val: a list
#'
#' $raw: a character string
#'
#' Note that $print, $new invisibly return itself and support method chaining.
#'
#' @export
Set <- R6::R6Class('Set',
                   public = list(
                     initialize = function(..., .list){
                       if (missing(.list)) .list <- list(...)

                       private$.val <- .list
                       invisible(self)
                     },

                     print = function(){
                       prefix <- '{'
                       suffix <- '}'
                       message('A set \n')
                       print(private$.val)
                     },
                     include = function(x){
                       if (is.null(x)) return(any(is_null(private$.val)))
                       sapply(x,
                              function(.x) {
                                if (is.null(.x)) return(any(is_null(private$.val)))
                                included = .x %in% private$.val
                                if (included)
                                  type.matched =
                                  any(typeof(.x) == typeof(unlist(private$.val[unlist(private$.val)==.x])))
                                else FALSE
                              },
                              USE.NAMES = FALSE)
                     }

                   ), private = list(
                     .val = list()
                   ), active = list(
                     raw = function(){
                       .val <- ifelse(is_character(private$.val),
                                      paste0('"', private$.val, '"'),
                                      private$.val)
                       el <- do.call(paste, as.list(c(.val, sep = ', ')))

                       paste0('{', el, '}')
                     },
                     val = function(){
                       return(private$.val)
                     }
                   ))



#' Set Generator
#' @description A wrapper of .Set$new()
#' @aliases as.set
#' @param ... a spreaded list of values.
#' @param .list an unspreaded list of values.
#' @export
as_set <- as.set <- function(..., .list){
  if (!missing(.list)) Set$new(.list=.list) else Set$new(...)
}

TimeEvent <-
  R6::R6Class('TimeEvent',
              public = list(
                initialize = function(start, end, mode){
                  if (!(is.double(start))) stop('start much be of type double')
                  if (!(is.double(end))) stop('end much be of type double')

                  private$.range <- ranger306:::.NumRange$new(as.numeric(start), as.numeric(end), mode)
                  private$.class <- class(start)
                },
                print = function(){
                  message('A time event:')
                  range <- private$get_range()
                  mode <- range$mode
                  prefix <- if (mode[[1]] == 'min') '[' else '('
                  suffix <- if (mode[[2]] == 'max') ']' else ')'
                  cat(prefix,
                      as.character(range$lower),
                      ',',
                      as.character(range$upper),
                      suffix)
                },
                include = function(x){
                  private$.range$include(x)
                },
                join_with = function()
              ),
              private = list(
                .range = NA,
                .class = NA,
                restore_class = function(x){
                  as.Date(x, origin = '1970-01-01')
                },
                get_range = function(range = private$.range){
                  lower <- private$restore_class(range$lower)
                  upper <- private$restore_class(range$upper)
                  mode <- range$Mode
                  return(list(lower=lower, upper=upper, mode=mode))
                }
              ))


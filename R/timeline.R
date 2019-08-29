Event <- R6::R6Class('Event',
                     inherit = 'NumRange',
                     public = list(
                       initialize = function(start, end){
                         event <- c(start, end)
                         if (!all(sapply(event, is.double))) stop('Should be either of type Date, POSIX, or double.')
                         private$.events <- event
                       },
                       print = function(raw = FALSE){
                         if (raw) return(print(private$.data))
                         dt <- data.frame(Events =  names(private$.data),
                                          Time = private$.data)
                         rownames(dt) <- NULL
                         return(print(dt))
                       },
                       # add = function(...){
                       #   threedots <- c(...)
                       #   if (!all(sapply(threedots, is.double))) stop('Should be either of type Date, POSIX, or double.')
                       #   private$.data <-  c(private$.data, threedots)
                       #   invisible(self)
                       # },
                       cut = function(...){

                       }
                     ),
                     private = list(
                       .data = date(),
                       .class = NA,
                       .
                     ))

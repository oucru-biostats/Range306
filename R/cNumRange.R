#' Combined Continuous Numeric Range
#'
#' @description An R6 object for combined continuous numeric ranges
#' @usage
#' cNumRange
#'  $new(..., list)
#'  $clone()
#'  $include(x)
#'  $cut(..., mode)
#'  $extract(id)
#'  $del(range)
#'  $print()
#'
#' @param ...
#' For method 'new': spreaded list of NumRange.
#' For method 'cut': spreaded list of split points
#' @param id the number of element NumRange within the object. If left missing, the method will return all elements.
#' @param list A list of NumRange. Used in case NumRanges are in a form of list.
#' @param mode Mode for returned cutted NumRanges, if missing, defaults of element NumRanges are used.
#' @param range A range that will be deleted from the host range.
#' @param x A numeric vector to check for inclusion.
#'
#' @return An object of R6Class "cNumRange"
#' @details
#' This class is used when simple NumRange is not enough.
#' It combines many NumRange into one, thus form a better constraint for complex data.
#' You can take back the simple NumRanges as its elements by calling the extract method.
#'
#' Please refer to NumRange for more details.
#' @importFrom magrittr %>%
#' @export
NumRange <-
  R6::R6Class('NumRange',
              public = list(
                initialize = function(..., .list, simplify=TRUE){
                  params <- if (missing(.list)) list(...) else .list
                  private$.list <-
                    sapply(params,
                           function(param){
                             if (is.null(param)) return(NULL)
                             if (is_.NumRange(param)) return(param)
                             if (is_NumRange(param)) return(param$extract())
                             else return(.NumRange$new(text=param))
                           }, USE.NAMES = FALSE) %>% unlist
                  if (simplify) private$.list <- self$simplify() %>%
                    (function(res) if (is_NumRange(res)) res$extract() else list(res))
                  invisible(self)
                },
                print = function(){
                  NRList <- private$.list
                  printList <-
                    lapply(NRList,
                           function(nr) nr$raw)
                  printList[is_null(printList)] <- '\U03A6'
                  printList$sep <- ' \U1D1C '
                  cat('A numeric range:',
                      do.call(paste, printList),
                      '\n')
                  invisible(self)
                },
                include = function(x){
                  NRList <- private$.list
                  res <-
                    lapply(NRList,
                           function(nr) nr$include(x))
                  res$FUN <- any
                  do.call(mapply, res)
                },
                extract = function(id){
                  if(missing(id)) return(private$.list)
                  if (length(id) == 1) return(private$.list[[id]])
                  return(private$.list[id])
                },
                cut = function(..., mode){
                    cutPts <- unique(list(...))
                    if (!missing(mode)) cutPts$mode <- mode

                  out <-
                    sapply(private$.list,
                           function(NR){
                             return(do.call(NR$cut, cutPts))
                           }) %>% unlist

                  return(out)
                },
                del = function(range){
                  out <-
                    sapply(private$.list,
                           function(NR){
                             return(NR$del(range))
                           }) %>% unlist

                  return(NumRange$new(.list=out))
                },
                simplify = function(){
                  all.NRs <- private$.list
                  out <- all.NRs[[1]]
                  if (length(all.NRs) > 1)
                    for (i in 2:length(all.NRs))
                      out <- out$union_with(all.NRs[[i]])
                  return(out)
                },
                includeRange = function(range){
                  intersectRange <- self$intersect_with(range)
                  is_identicalRange(self, intersectRange)
                },
                intersect_with = function(range){
                  if (is.character(range)) range <- .NumRange$new(text=range)
                  NRs <- if (is_.NumRange(range)) list(range) else range$extract()
                  self.NRs <- private$.list
                  NR.grid <- expand.grid(self.NRs, NRs)
                  row.intersect <- sapply(nrow(NR.grid),
                                          function(i) NR.grid[[i, 1]]$intersect_with(NR.grid[[i,2]])$extract())
                  NumRange$new(.list=row.intersect)
                },
                union_with = function(range){
                  if (is.character(range)) range <- .NumRange$new(text=range)
                  # all.NRs <- c(private$.list, if (is_.NumRange(range)) range else range$extract())
                  NRs <- if (is_.NumRange(range)) list(range) else range$extract()
                  self.NRs <- private$.list
                  all.NRs <- NULL
                  free <- rep(TRUE, length(NRs))
                  self.free <- rep(TRUE, length(self.NRs))
                  for (self.NR in self.NRs){
                    unioned <- 0

                    for (NR in NRs) {
                      if (!unioned){
                        if (!is_NullRange(NR$intersect_with(self.NR))){
                          all.NRs <- c(all.NRs, self.NR$union_with(NR))
                          self.free[sapply(self.NRs, function(.self.NR) is_identicalRange(NumRange$new(.self.NR), NumRange$new(self.NR)))] <- FALSE
                          free[sapply(NRs, function(.NR) is_identicalRange(NumRange$new(.NR), NumRange$new(NR)))] <- FALSE
                          unioned <- 1
                        }
                      }
                    }
                  }
                  all.NRs <- c(self.NRs[self.free], all.NRs, NRs[free])
                  return(NumRange$new(.list=all.NRs, simplify = FALSE))
                }
              ),
              private = list(
                .list = NULL
              ))


#' Check if an object is of class cNumRange.
#'
#' @aliases is.cNumRange
#' @param x An object to check
#'
#' @return A logical vector of length==length(x)
#' @export
is_NumRange <- is.NumRange <- function(x){
  is_(x, 'NumRange')
}

is_NullRange <- is.NullRange <- function(x){
  if (!is_(x, 'NumRange')) stop('x should be of class NumRange')
  all(is_null(x$extract()))
}

is_identicalRange <- function(x, y){
  if (!is_(x, 'NumRange')) stop('x should be of class NumRange')
  if (!is_(y, 'NumRange')) stop('y should be of class NumRange')

  x <- x$extract()
  y <- y$extract()
  NR.grid <- expand.grid(x, y)
  all(sapply(seq_along(nrow(NR.grid)),
             function(i){
               .x <- NR.grid[[i, 1]]
               .y <- NR.grid[[i, 2]]
               .x$lower == .y$lower & .x$upper == .y$upper & identical(.x$Mode, .y$Mode)
             }))
}



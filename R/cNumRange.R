#' R6 Class Generator for Continuous Numeric Range
#'
#' @description An R6 object for combined continuous numeric ranges
#' @usage
#' NumRange
#'  $new(..., list)
#'  $clone()
#'  $include(x)
#'  $cover(range)
#'  $covered_by(range)
#'  $cut(..., mode)
#'  $extract(id)
#'  $intersect_with(range)
#'  $union_with(range)
#'  $antiunion_with(range)
#'  $print()
#'
#' @method $print
#' @param ...
#' For method 'new': spreaded list of NumRange.
#' For method 'cut': spreaded list of split points
#' @param id the number of element NumRange within the object. If left missing, the method will return all elements.
#' @param list A list of NumRange. Used in case NumRanges are in a form of list.
#' @param mode Mode for returned cutted NumRanges, if missing, defaults of element NumRanges are used.
#' @param range A range that will be deleted from the host range, a valid character input can also be converted into NumRange, for convenience.
#' @param x A numeric vector to check for inclusion.
#'
#' @return An object of R6Class "NumRange"
#' @details
#' This function create a numeric range, via which you can check if a specific number is included or not.
#'
#' After creation, you can use some built-in methods.
#'
#' - $include will check if some provided numbers are included inside a range.
#'
#' - $arrrange will create a sequence for the range with a definded step.
#'
#' - $cut will cut the range into small range at the cutting point.
#'
#' - $print will print the range in a beautified way.
#'
#' - $union_with, $antiunion_with, and $intersect_with do union, anti_union, and intersect with a range.
#'
#' - $cover and $covered_by: a range is covered by another if all of it elements are included in the later.
#'
#' Note that $new, $include, $print(raw = FALSE) will accept method chaining.
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
                arrrange = function(step, simplify=FALSE){
                  if (missing(step))
                    out <- lapply(private$.list,
                                  function(NR) NR$arrrange())
                  else {
                    step <- rep(step, length(private$.list))
                    out <- lapply(seq_along(private$.list),
                                  function(i) private$.list[[i]]$arrrange(step[[i]]))
                  }
                  if (simplify) return(unlist(out))
                  return(out)
                },
                antiunion_with = function(range){
                  out <-
                    sapply(private$.list,
                           function(NR){
                             return(NR$antiunion_with(range))
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
                covered_by = function(range){
                  if (is.character(range)) range <- NumRange$new(range)
                  range$cover(self)
                },
                cover = function(range){
                  if (is.character(range)) range <- NumRange$new(range)
                  intersectRange <- self$intersect_with(range)
                  is_identicalRange(intersectRange, range)
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

#' Check if a numeric range is empty
#'
#' @aliases is.NullRange
#' @param x a numeric range of R6Class 'NumRange'
#' @return a logical value
is_EmptyRange <- is.EmptyRange <- function(x){
  if (!inherits(x, 'NumRange')) stop('x should be of class NumRange')
  all(is_null(x$split()))
}

#' Check if two numeric ranges are identical
#' @aliases is.identicalRange
#' @param x,y two numeric ranges of R6Class 'NumRange'
#' @return a logical value
is_identicalRange <- is.identicalRange <- function(x, y){
  if (!is_(x, 'NumRange')) stop('x should be of class NumRange')
  if (!is_(y, 'NumRange')) stop('y should be of class NumRange')

  x <- x$simplify()$extract()
  y <- y$simplify()$extract()

  all(
    sapply(x,
           function(.x){
             any(sapply(y,
                    function(.y){
                      .x$lower == .y$lower && .x$upper == .y$upper && .x$Mode == .y$Mode
                    }))
           }),
    length(x) == length(y)
  )
}

#' Union 2 NumRange objects
#' @description A wrapper to union 2 NumRange object, which is in fact a wrapper for R6 method $union_with
#' @param x,y Two NumRange objects to be unioned.
#' @return an object of R6Class 'NumRange'
#' @export
NR_union <- function(x, y){
  if (is.character(x)) x <- NumRange$new(x)
  if (is.character(y)) y <- NumRange$new(y)

  x$union_with(y)
}

#' Intersect of 2 NumRange objects
#' @description A wrapper for R6 method NumRange$intersect_with
#' @param x,y Two NumRange objects to be intersected
#' @return an object of R6Class 'NumRange'
#' @export
NR_intersect <- function(x, y){
  if (is.character(x)) x <- NumRange$new(x)
  if (is.character(y)) y <- NumRange$new(y)

  x$intersect_with(y)
}

#' Anti union 2 NumRange objects
#' @description A wrapper for R6 method NumRange$antiunion_with
#' @param x,y Two NumRange objects to be anti-unioned
#' @return an object of R6Class 'NumRange'
#' @export
NR_antiunion <- function(x, y){
  if (is.character(x)) x <- NumRange$new(x)
  if (is.character(y)) y <- NumRange$new(y)

  x$antiunion_with(y)
}

#' Check if a numeric range is covered within another one
#' @aliases is.covered_by
#' @description A wrapper for R6 method NumRange$covered_by
#' @param x A vector or object of R6class NumRange to be check, a valid character vector can be converted into NumRange object, for convenience.
#' @param range A NumRange object, a valid character vector can be converted into NumRange object, for convenience.
#' @return A logical vector
#' @export
is_covered_by <- is.covered_by <- function(x, range){
  if (is.character(x)) x <- NumRange$new(x)
  x$covered_by(range)
}

#' Check if a numeric range cover another one
#' @description A wrapper for R6 method NumRange$cover
#' @param x A vector or object of R6class NumRange to be check, a valid character vector can be converted into NumRange object, for convenience.
#' @param range A NumRange object, a valid character vector can be converted into NumRange object, for convenience.
#' @return A logical vector
#' @export
is_covered_by <- is.covered_by <- function(range, x){
  if (is.character(range)) x <- NumRange$new(range)
  range$cover(x)
}

#' Populate a numeric range into discrete sequence
#' @description A wrapper for R6 method NumRange$arrrange
#' @param range A NumRange object, a valid character vector can be converted into NumRange object, for convenience.
#' @param step A vector of steps for each member range. Default is each range's (max-min)/10. Length of step should be 1 or number of member ranges.
#' @param simplify Default is FALSE. TRUE for automatically unlist the result vector.
#' @return A list of sequence if simiplify = FALSE or a sequence otherwise.
#' @export
arrrange <- function(range, step, simplify=FALSE){
  if (is.character(range)) range <- NumRange$new(range)
  if (!missing(step)) return(range$arrrange(step, simplify = simplify))
  return(range$arrrange(simplify=simplify))
}

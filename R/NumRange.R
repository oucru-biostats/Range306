#'Class Num
#'
#' @export
#'
NumRange <-
  R6::R6Class('NumRange',
              public = list(
                initialize = function(lower, upper, bound, list, simplify = TRUE){
                  formal <- if (!missing(lower) & !missing(upper) & !missing(bound))
                    private$.parse_formal(lower, upper, bound)
                  informal <- if (!missing(list)) private$.parse_list(list)

                  parsed <- c(formal, informal)
                  private$.atomic <- parsed
                  if (simplify) private$.atomic <- self$simplify()$.__enclos_env__$private$.atomic

                  return(self)
                },
                print = function(){
                  AtomicList <- private$.atomic
                  if (length(AtomicList) == 1) return(AtomicList[[1]]$print())
                  printList <-
                    lapply(AtomicList,
                           function(ANR) ANR$as.character())
                  if (!length(printList)) printList <- list('\U03A6')
                  printList$sep <- ' \U1D1C '
                  cat('A numeric range:',
                      do.call(paste, printList),
                      '\n')
                  invisible(self)
                },
                split = function(){
                  lapply(private$.atomic,
                         function(ANR) {
                           NumRange$new(list=ANR)
                         })
                },
                simplify = function(){
                  all.NRs <- private$.atomic
                  out <- all.NRs[[1]]
                  if (length(all.NRs) > 1)
                    for (i in 2:length(all.NRs))
                      out <- out$union(all.NRs[[i]])
                  if (inherits(out, 'AtomicNumRange')) out <- NumRange$new(list=out, simplify = FALSE)
                  return(out)
                },
                identical = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text=range)
                  ANRs <- list()
                  if (inherits(range, 'NumRange'))
                    ANRs <- range$.__enclos_env__$private$.atomic
                  if (inherits(range, 'AtomicNumRange')){
                    ANRs <- list(range)
                    # if (!inherits(ANRs, 'AtomicNumRange')) stop('Invalid range!')
                    # ANRs <- list(ANRs)
                  }

                  self.ANRs <- private$.atomic
                  all(
                    length(self.ANRs) == length(ANRs),
                    sapply(self.ANRs,
                          function(self.ANR)
                             any(sapply(ANRs,
                                        function(ANR)
                                          self.ANR$identical(ANR))))
                  )
                },
                union = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text=range)
                  ANRs <- list()
                  if (inherits(range, 'NumRange'))
                    ANRs <- range$.__enclos_env__$private$.atomic
                  if (inherits(range, 'AtomicNumRange')){
                    ANRs <- list(range)
                    # if (!inherits(ANRs, 'AtomicNumRange')) stop('Invalid range!')
                    # ANRs <- list(ANRs)
                  }

                  self.ANRs <- private$.atomic
                  all.ANRs <- NULL
                  free <- rep(TRUE, length(ANRs))
                  self.free <- rep(TRUE, length(self.ANRs))
                  for (self.ANR in self.ANRs){
                    unioned <- 0

                    for (ANR in ANRs) {
                      if (!unioned){
                        if (!is_EmptyRange(ANR$intersect(self.ANR))){
                          all.ANRs <- c(all.ANRs, self.ANR$union(ANR))
                          self.free[sapply(self.ANRs,
                                           function(.self.ANR)
                                             is_identicalRange(NumRange$new(list=.self.ANR),
                                                               NumRange$new(list=self.ANR)))] <- FALSE
                          free[sapply(ANRs,
                                      function(.ANR)
                                        is_identicalRange(NumRange$new(list=.ANR),
                                                          NumRange$new(list=ANR)))] <- FALSE
                          unioned <- 1
                        }
                      }
                    }
                  }
                  all.ANRs <- c(self.ANRs[self.free], all.ANRs, ANRs[free])
                  return(NumRange$new(list=all.ANRs, simplify = FALSE))
                },
                anti_union = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text=range)

                  out <-
                    unlist(sapply(private$.atomic,
                           function(ANR){
                             return(ANR$anti_union(range))
                           }))

                  return(NumRange$new(list=out, simplify = FALSE))
                },
                is_EmptyRange = function(){
                  ANRs <- private$.atomic
                  if (!length(ANRs)) return(TRUE)
                  all(sapply(ANRs, function(ANR) ANR$is_EmptyRange()))
                },
                include = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text=range)
                  if (self$is_EmptyRange()) return(FALSE)
                  self.ANRs <- self$simplify()
                  ANRs <- self.ANRs$.__enclos_env__$private$.atomic
                  out <-
                    sapply(ANRs,
                           function(ANR)
                             ANR$include(range)
                    )
                  return(any(out))
                },
                cut = function(...,at, bound){
                  if (missing(at)) at <- c(...)
                  missing.bound <- missing(bound)
                  out <- unlist(
                    lapply(private$.atomic,
                           function(ANR)
                             if (missing.bound) ANR$cut(at=at) else ANR$cut(at=at, bound=bound))
                  )
                  return(out)
                },
                arrrange = function(step){
                  missing.step = missing(step)
                  if (!is.numeric(step)) stop('step must be numeric')
                  self.simplify <- self$simplify()
                  out <-
                    unlist(
                      lapply(self.simplify$.__enclos_env__$private$.atomic,
                             function(ANR)
                               if (missing.step) ANR$arrrange() else ANR$arrrange(step)
                      )
                    )
                  return(out)
                }
              ),
              private = list(
                #props
                .atomic = list(),

                #methods
                .match_bound = function(bound){
                  c(c('min','inf')[c('min', 'inf') %in% bound],
                    c('max','sup')[c('max', 'sup') %in% bound])
                },
                .valid_formal = function(lower, upper, bound){
                  if (!is.numeric(lower)) stop('lower should be numeric!')
                  if (!is.numeric(upper)) stop('upper should be numeric!')
                  if (lower > upper) stop('Invalid range!')
                  valid.bound <- {
                    l <- length(bound)
                    if (l>2 | l<1) return(FALSE)
                    if (l==1) return(bound %in% c('inclusive', 'exclusive'))
                    return(
                      length(intersect(bound, c('inf', 'min'))) &
                        length(intersect(bound, c('sup', 'max')))
                    )
                  }

                  if(!valid.bound) stop('Invalid boundary mode!')
                  invisible(TRUE)
                },
                .parse_formal = function(lower, upper, bound){
                  if (length(lower) != length(upper))
                    stop('Length of lower and upper must match!')
                  if (!length(bound) %in% c(1, 2, length(lower)))
                    stop('bound must be of length 1 or that of the boundaries!')
                  if (length(bound) %in% c(1,2) & is.atomic(bound))
                    bound <- rep(list(bound), length(lower))
                  if (length(bound)<length(lower))
                    stop('bound is not in preferable form.')
                  parsed <-
                    lapply(seq_along(lower),
                           function(i){
                             .lower <- lower[[i]]
                             .upper <- upper[[i]]
                             .bound <- bound[[i]]
                             AtomicNumRange$new(.lower, .upper, .bound)
                           })

                  return(parsed)
                },
                .parse_list = function(ls){
                  if (inherits(ls, 'AtomicNumRange')) return(ls)
                  if (inherits(ls, 'NumRange')) return(private$.parse_NumRange(ls))
                  unlist(lapply(ls,
                         function(.ls){
                           if (inherits(.ls, 'NumRange')) return(private$.parse_NumRange(.ls))
                           if (inherits(.ls, 'AtomicNumRange')) return(.ls)
                           if (is.character(.ls)) return(AtomicNumRange$new(text = .ls))
                           return(NULL)
                         }))
                },
                .parse_NumRange = function(NRObj){
                  AtomicList <- NRObj$.__enclos_env__$private$.atomic
                  return(AtomicList)
                },
                .include_range = function(range){
                  if (is.character(range))
                    range <- NumRange$new(range)
                  intersectRange <- self$intersect(range)
                  is_identicalRange(intersectRange, range)
                }
              ))

# is_identicalRange <- is.identicalRange <- function(x, y){
#   if (!inherits(x, 'NumRange')) stop('x should be of class NumRange')
#   if (!inherits(y, 'NumRange')) stop('y should be of class NumRange')
#
#   x <- x$simplify()$.__enclos_env__$private$.atomic
#   y <- y$simplify()$.__enclos_env__$private$.atomic
#
#   all(
#     sapply(x,
#            function(.x){
#              any(sapply(y,
#                         function(.y){
#                           .x$lower == .y$lower && .x$upper == .y$upper && .x$bound == .y$bound
#                         }))
#            }),
#     length(x) == length(y)
#   )
# }

R62S3::R62S3(NumRange, assignEnvir = parent.frame())

#' R6 Class Generator for Continuous Numeric Range
#'
#' @description An R6 object for continuous numeric ranges
#' @usage
#' NR
#'  $new(lower, upper, mode='inclusive', text)
#'  $include(x)
#'  $arrrange(step=(upper-lower)/10)
#'  $cut(..., mode)
#'  $print(raw = FALSE)
#'  $raw
#'  $Mode
#'
#' @param lower Lower limit of the range
#' @param upper Upper limit of the range
#' @param mode A vector of length 2 declaring modes for lower and upper. Default is "inclusive". Modes for lower can be "min" or "inf" for lower-inclusive and -exclusive, respectively. Mode for upper can be "max" or "sup" for upper-inclusive or -exclusive. Shorthand for mode = c("min", "max") is "inclusive"; and shorthand for mode = c("inf", sup") is "exclusive".
#' @param text A syntax-aware string to be convert to NumRange. See Details.
#' @param x A numeric vector
#' @param step The step of the new sequence. Similar to step parameter in seq()
#' @param ... All the split points at which the range will be cut.
#' @param raw If TRUE, similar to $raw
#'
#' @return An object of R6Class="NumRange"
#' @details
#' This function create a numeric range, via which you can check if a specific number is included or not.
#' One formal one to create a new range is via method $new(lower, upper, mode) where mode defines how the range boundaries will behave.
#' Mode is a length-two vector defining the each boundaries'mode.
#'
#' Accepted modes for lower limit are "inf" for exclusive lower bound and "min" for lower limit inclusion.
#' Accepted modes for upper limit are "sup" for exclusive upper bound and "max" for upper limit inclusion.
#'
#' Shorthand for mode = c('min', 'max') is 'inclusive'.
#' Shorthand for mode = c('inf', 'sup') is 'exclusive'.
#'
#' Another way to create a NumRange is via text=string conversion.
#' Character string should be in a form "(a,b)", "[a,b)", "(a, b]", or "(a, b)".
#'
#' After creation, you can use some built-in methods.
#'
#' - $include will check if some provided numbers are included inside a range.
#'
#' - $arrange will create a sequence for the range with a definded step.
#'
#' - $cut will cut the range into small range at the cutting point.
#'
#' - $print will print the range in a beautified way.
#'
#' - $raw will return a string that contains the range.
#'
#' - $Mode will return the provided mode of the range.
#'
#' Note that $new, $include, $print(raw = FALSE) will accept method chaining.
#'
#' @export
.NumRange <- R6::R6Class('.NumRange',
                        public = list(
                          lower = NA,
                          upper = NA,
                          initialize =  function(lower, upper, mode='inclusive', text){
                            if (!missing(lower) & !missing(upper)){
                              if (!(is.numeric(lower) & is.numeric(upper))) stop('Lower and upper must be numeric!')
                              if (length(mode) == 1){
                                if (!mode %in% c('inclusive', 'exclusive', 'sup', 'max', 'inf', 'min')) stop('Invalid mode!')
                                if (mode %in% c('min', 'sup')) mode <- c('min', 'sup')
                                else if (mode %in% c('inf', 'max')) mode <- c('inf', 'max')
                                else if (mode == 'inclusive') mode <- c('min', 'max')
                                else if (mode == 'exclusive') mode <- c('inf', 'sup')
                              } else if (length(mode) != 2 |
                                         !length(intersect(mode, c('inf','min' ))) |
                                         !length(intersect(mode, c('sup', 'max')))) stop ('Invalid mode!')

                              if (is.infinite(upper)) mode[2] <- 'sup'

                              if (lower > upper) stop ('Invalid or trivial range!')

                              private$mode <- c(mode[mode %in% c('inf', 'min')], mode[mode %in% c('sup', 'max')])

                              if (lower == upper){
                                if (!identical(mode, c('min', 'max'))) stop('Invalid or trivial range!')
                                private$mode <- 'collapsed'
                              }

                              self$lower <- lower
                              self$upper <- upper
                            } else {
                              if (missing(text)) stop('Missing input!')
                              prefix <- substr(text, 1, 1)
                              suffix <- substr(text, nchar(text), nchar(text))
                              if (!prefix %in% c('[', '(') | !suffix %in% c(']', ')'))
                                stop('Invalid string input!')
                              num <- substr(text, 2, nchar(text)-1)
                              num.split <- strsplit(num, '\\s?[,;]\\s?', perl=TRUE)[[1]]
                              lower <- as.numeric(num.split[1])
                              upper <- as.numeric(num.split[2])

                              mode.1 <- if (prefix == '[') 'min' else 'inf'
                              mode.2 <- if (suffix == ']') 'max' else 'sup'

                              self$initialize(lower, upper, mode = c(mode.1, mode.2))
                            }

                            invisible(self)
                          },

                          print = function(raw=FALSE){
                            if (raw) return(self$raw)
                            if (all(private$mode == 'collapsed')) {
                              cat('A collapsed range:',self$lower, '\n')
                            } else {
                              lead <- if ('min' %in% private$mode) '[' else '('
                              trail <- if ('max' %in% private$mode) ']' else ')'

                              cat('A numeric range:', paste0(lead, self$lower, ', ', self$upper, trail), '\n')
                            }
                            invisible(self)
                          },

                          include = function(x){
                            sapply(x, function(.x) {
                              if (is.na(.x)|is.null(.x)) return(FALSE)
                              if (!is.numeric(.x) & is.logical(.x)) stop('x must be numeric!')
                              lower.ok <- if ('min' %in% private$mode | 'collapsed' %in% private$mode) .x >= self$lower else .x > self$lower
                              upper.ok <- if ('max' %in% private$mode | 'collapsed' %in% private$mode) .x <= self$upper else .x < self$upper
                              lower.ok & upper.ok
                            })
                          },

                          arrrange = function(step=(self$upper - self$lower)/10){
                            if (!is.numeric(step)) stop('step must be numeric!')

                            seq.gen <- seq(self$lower, self$upper, step)
                            if (!'min' %in% private$mode)  seq.gen <- seq.gen[-1]
                            if (!'max' %in% private$mode)  seq.gen <- seq.gen[-length(seq.gen)]
                            return(seq.gen)
                          },

                          cut = function(..., mode = private$mode){
                            cutPts <- unique(c(..., self$upper, self$lower))
                            cutPts <- sort(cutPts[cutPts <= self$upper & cutPts >= self$lower])

                            if (length(mode) == 1){
                              if (!mode %in% c('inclusive', 'exclusive', 'sup', 'max', 'inf', 'min')) stop('Invalid mode!')
                              if (mode %in% c('min', 'sup')) mode <- c('min', 'sup')
                              else if (mode %in% c('inf', 'max')) mode <- c('inf', 'max')
                              else if (mode == 'inclusive') mode <- c('min', 'max')
                              else if (mode == 'exclusive') mode <- c('inf', 'sup')
                            } else if (length(mode) != 2 |
                                       !length(intersect(mode, c('inf','min' ))) |
                                       !length(intersect(mode, c('sup', 'max')))) stop ('Invalid mode!')

                            modes <- rep(list(mode), length(cutPts)-1)
                            modes[[1]] <- c(private$mode[1], modes[[1]][2])
                            modes[[length(modes)]] <- c(modes[[length(modes)]][1], private$mode[2])
                            lapply(1:(length(cutPts)-1), function(i){
                              lower <- cutPts[i]
                              upper <- cutPts[i+1]
                              NumRange$new(.NumRange$new(lower, upper, modes[[i]]))
                            })
                          },

                          antiunion_with = function(range){
                            if (is_.NumRange(range)) return(private$.del(range))
                            if (is.character(range)) return(private$.del(.NumRange$new(text=range)))
                            warning('Unrecognized format. NA returned.')
                            NA
                          },
                          intersect_with = function(range){
                            if (is.character(range)) range <- .NumRange$new(text=range)

                            lower <- range$lower
                            upper <- range$upper
                            mode <- range$Mode
                            self.mode <- private$mode
                            if ('collapsed' %in% mode) mode <- c('min', 'max')
                            else mode <- c(mode[mode %in% c('min', 'inf')], mode[mode %in% c('max', 'sup')])
                            if ('collapsed' %in% self.mode) self.mode <- c('min', 'max')
                            else self.mode <- c(self.mode[self.mode %in% c('min', 'inf')], self.mode[self.mode %in% c('max', 'sup')])

                            if (any(
                              upper < self$lower || lower > self$upper,
                              lower == self$upper && (mode[1] != 'min' || self.mode[2] !='max'),
                              upper == self$lower && (mode[2] != 'max' || self.mode[1] != 'min')
                              ))
                              return(NumRange$new(NULL))

                            else {
                              allRange <- sort(c(lower, upper, self$lower, self$upper))
                              newLower <- allRange[2]
                              newUpper <- allRange[3]
                              mode.lower <- c(mode[1], self.mode[1]) %>% `[`(c(lower, self$lower) == newLower)
                              mode.upper <- c(mode[2], self.mode[2]) %>% `[`(c(upper, self$upper) == newUpper)

                              if (length(mode.lower)==2 & 'min' %in% mode.lower) mode.lower <- 'min' else 'inf'
                              if (length(mode.upper)==2 & 'max' %in% mode.upper) mode.upper <- 'max' else 'sup'

                              return(NumRange$new(.NumRange$new(newLower, newUpper, c(mode.lower, mode.upper))))
                            }
                          },

                          union_with = function(range){
                            if (is.character(range)) range <- .NumRange$new(text=range)
                            intersectRange <- self$intersect_with(range)


                            lower <- range$lower
                            upper <- range$upper
                            mode <- range$Mode
                            self.mode <- private$mode

                            if ('collapsed' %in% mode) mode <- c('min', 'max')
                            else mode <- c(mode[mode %in% c('min', 'inf')], mode[mode %in% c('max', 'sup')])
                            if ('collapsed' %in% self.mode) self.mode <- c('min', 'max')
                            else self.mode <- c(self.mode[self.mode %in% c('min', 'inf')], self.mode[self.mode %in% c('max', 'sup')])

                            if (is_NullRange(intersectRange) &&
                                ((lower == self$upper && mode[[1]] == 'inf' && self.mode[[2]] == 'sup') ||
                                 (upper == self$lower && (mode[[2]] == 'sup' || self.mode[[1]] == 'inf'))))
                              return(NumRange$new(self, range, simplify = FALSE))

                            allRange <- sort(c(lower, upper, self$lower, self$upper))
                            newLower <- allRange[1]
                            newUpper <- allRange[4]

                            mode.lower <- c(mode[1], self.mode[1]) %>% `[`(c(lower, self$lower) == newLower)
                            mode.upper <- c(mode[2], self.mode[2]) %>% `[`(c(upper, self$upper) == newUpper)

                            if (length(mode.lower)==2 & 'min' %in% mode.lower) mode.lower <- 'min' else 'inf'
                            if (length(mode.upper)==2 & 'max' %in% mode.upper) mode.upper <- 'max' else 'sup'
                            return(NumRange$new(.NumRange$new(newLower, newUpper, c(mode.lower, mode.upper))))
                          }
                        ),
                        private = list(
                          mode = character(0),
                          .del = function(NR){
                            upper <- NR$upper
                            lower <- NR$lower

                            if (lower > self$upper | upper < self$lower) {
                              return(self)
                            }

                            mode <- c(NR$Mode[NR$Mode %in% c('inf', 'min')], NR$Mode[NR$Mode %in% c('sup', 'max')])
                            self.mode <- c(private$mode[private$mode %in% c('inf', 'min')],
                                           private$mode[private$mode %in% c('sup', 'max')])
                            if (lower <= self$lower) {
                              mode[1] <- self.mode[1]
                              lower <- self$lower
                            }
                            if (upper >= self$upper) {
                              mode[2] <- self.mode[2]
                              upper <- self$upper
                            }

                            antiMode <- character(0)
                            antiMode[1] <- if (mode[1] == 'inf') 'max' else 'sup'
                            antiMode[2] <- if (mode[2] == 'sup')'min' else 'inf'
                            new.1 <- new.2 <- NULL

                            if (lower != self$lower || (self.mode[1] != 'inf' && antiMode != 'sup'))
                              new.1 <- .NumRange$new(self$lower, lower, mode = c(self.mode[1], antiMode[1]))
                            if (upper != self$upper || (self.mode[2] != 'sup' && antiMode != 'inf'))
                              new.2 <- .NumRange$new(upper, self$upper, mode = c(antiMode[2], self.mode[2]))

                            if (length(new.1) && length(new.2)) return(NumRange$new(.NumRange$new(new.1, new.2)))
                            else if (length(new.1)) return(NumRange$new(new.1))
                            else if (length(new.2)) return(NumRange$new(new.2))
                          }),
                        active = list(
                          raw = function(){
                            if (all(private$mode == 'collapsed')) {
                              return(self$lower)
                            } else {
                              lead <- if ('min' %in% private$mode)  '[' else '('
                              trail <- if ('max' %in% private$mode)  ']' else ')'

                              return(paste0(lead, self$lower, ', ', self$upper, trail))
                            }
                          },
                          Mode = function(){
                            private$mode
                          }
                        ))

#' Convert a character string to object NumRange.
#'
#' @aliases as.NumRange
#' @param x
#' An vector of type character.
#' x should be in the form of bracketed string, leaded by "[" or "(", and ended with ")" or "]".
#'
#' Other form won't be accepted.
#'
#' @export
as_.NumRange <- as..NumRange <- function(x){
  .NumRange$new(text=x)
}

#' Check if an object is of class NumRange.
#'
#' @aliases is.NumRange
#' @param x An object to check
#' @return A logical vector of length==length(x)
#'
#' @export
is_.NumRange <- is..NumRange <- function(x){
  ".NumRange" %in% class(x)
}




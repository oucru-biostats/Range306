#"Atomic
#' @export
AtomicNumRange <-
  R6::R6Class('AtomicNumRange',
              public = list(
                initialize =
                  function(lower, upper, bound = 'inclusive', text){
                    parsed <-
                      if (!missing(lower) & !missing(upper))
                        private$.parse_formal(lower, upper, bound)
                    else text <- private$.parse_str(text)

                    private$.lower <- parsed$lower
                    private$.upper <- parsed$upper
                    private$.bound <- parsed$bound

                    return(self)
                  },
                print =
                  function(){

                    if (private$.lower == private$.upper) {
                      cat(paste0('A collapsed range: [', private$.lower, ']\n'))
                    } else {
                      prefix <- if (private$.bound[1] == 'min') '[' else '('
                      suffix <- if (private$.bound[2] == 'max') ']' else ')'

                      cat('A numeric range:',
                          paste0(prefix, private$.lower, ', ', private$.upper, suffix,'\n'))
                    }

                    invisible(self)
                  },
                as.character = function(verbose = FALSE){
                  if (private$.lower == private$.upper) {
                    out <- paste0(if (verbose) 'A collapsed range: ' else NULL,
                                  '[', private$.lower, ']')
                  } else {
                    prefix <- if (private$.bound[1] == 'min') '[' else '('
                    suffix <- if (private$.bound[2] == 'max') ']' else ')'

                    out <-
                      paste0(if (verbose) 'A numeric range: ' else NULL,
                             paste0(prefix, private$.lower, ', ', private$.upper, suffix))
                  }

                  return(out)
                },
                include = function(x){
                  if (is.character(x)) x <- AtomicNumRange$new(text=x)
                  if (!is.list(x)) x <- list(x)
                  sapply(x, function(.x){
                    if (is.numeric(.x)) return(private$.include_numeric(.x))
                    if (inherits(.x,'AtomicNumRange')) return(private$.include_range(.x))
                  })
                },
                identical = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text=range)
                  if (!inherits(range, 'AtomicNumRange'))
                    stop('range is not a parsable object!')
                  return(
                    private$.lower == range$lower &&
                      private$.upper == range$upper &&
                      identical(private$.bound, range$bound)
                    )
                },
                arrrange = function(step = (private$.upper - private$.lower)/10){
                  if (!is.numeric(step)) stop('step must be numeric!')
                  if (!is.finite(private$.lower) | !is.finite(private$.upper))
                    stop('arrrange method is not valid for this unbounded range.')

                  seq.gen <- seq(private$.lower, private$.upper, step)
                  if (!'min' %in% private$.bound)  seq.gen <- seq.gen[-1]
                  if (!'max' %in% private$.bound)  seq.gen <- seq.gen[-length(seq.gen)]
                  return(seq.gen)
                },
                cut = function(..., at, bound = private$.bound){
                  if (missing(at)) at <- c(...)
                  cutPts <- unique(c(at, private$.upper, private$.lower))
                  cutPts <- sort(cutPts[cutPts <= private$.upper & cutPts >= private$.lower])

                  valid.bound <- private$.valid_bound(bound)

                  if(!valid.bound) stop('Invalid boundary mode!')

                  if (length(bound) == 1){
                    bound <-
                      if(bound == 'inclusive') c('min','max') else c('inf', 'sup')
                  } else bound <- private$.match_bound(bound)

                  bounds <- rep(list(bound), length(cutPts)-1)
                  bounds[[1]] <- c(private$.bound[1], bounds[[1]][2])
                  bounds[[length(bounds)]] <- c(bounds[[length(bounds)]][1], private$.bound[2])
                  lapply(1:(length(cutPts)-1), function(i){
                    lower <- cutPts[i]
                    upper <- cutPts[i+1]
                    AtomicNumRange$new(lower, upper, bounds[[i]])
                  })
                },
                intersect = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text = range)
                  if (!inherits(range, 'AtomicNumRange'))
                    stop('range is not a parsable object!')

                  lower <- range$lower
                  upper <- range$upper
                  bound <- range$bound
                  self.bound <- private$.bound

                  if (any(
                    upper < private$.lower || lower > private$.upper,
                    lower == private$.upper && (bound[1] != 'min' || self.bound[2] !='max'),
                    upper == private$.lower && (bound[2] != 'max' || self.bound[1] != 'min')
                  ))
                    return(NumRange$new(list=NULL, simplify=FALSE))

                  else {
                    allRange <- sort(c(lower, upper, private$.lower, private$.upper))
                    newLower <- allRange[2]
                    newUpper <- allRange[3]
                    bound.lower <- c(bound[1], self.bound[1])[(c(lower, private$.lower) == newLower)]
                    bound.upper <- c(bound[2], self.bound[2])[(c(upper, private$.upper) == newUpper)]

                    if (length(bound.lower)==2 & 'min' %in% bound.lower) bound.lower <- 'min' else 'inf'
                    if (length(bound.upper)==2 & 'max' %in% bound.upper) bound.upper <- 'max' else 'sup'

                    return(NumRange$new(lower=newLower,
                                        upper=newUpper,
                                        bound=c(bound.lower, bound.upper), simplify=FALSE))
                  }
                },
                union = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text = range)
                  if (!inherits(range, 'AtomicNumRange'))
                    stop('range is not a parsable object!')

                  lower <- range$lower
                  upper <- range$upper
                  bound <- range$bound
                  self.bound <- private$.bound
                  intersectRange <- self$intersect(range)

                  if (
                    is_EmptyRange(intersectRange) ||
                    (
                      (lower == private$.upper && bound[[1]] == 'inf' && self.bound[[2]] == 'sup') ||
                      (upper == private$.lower && (bound[[2]] == 'sup' || self.bound[[1]] == 'inf'))
                    )
                  )
                    return(NumRange$new(list=c(self, range), simplify = FALSE))

                  allRange <- sort(c(lower, upper, private$.lower, private$.upper))
                  newLower <- allRange[1]
                  newUpper <- allRange[4]

                  bound.lower <- c(bound[1], self.bound[1])[(c(lower, private$.lower) == newLower)]
                  bound.upper <- c(bound[2], self.bound[2])[(c(upper, private$.upper) == newUpper)]

                  if (length(bound.lower)==2 & 'min' %in% bound.lower) bound.lower <- 'min' else 'inf'
                  if (length(bound.upper)==2 & 'max' %in% bound.upper) bound.upper <- 'max' else 'sup'
                  return(NumRange$new(lower=newLower,
                                      upper=newUpper,
                                      bound=c(bound.lower, bound.upper), simplify=FALSE))
                },
                anti_union = function(range){
                  if (is.character(range))
                    range <- AtomicNumRange$new(text = range)
                  if (!inherits(range, 'AtomicNumRange'))
                    stop('range is not a parsable object!')

                  upper <- range$upper
                  lower <- range$lower

                  if (lower > private$.upper | upper < private$.lower) {
                    return(self)
                  }

                  bound <- range$bound
                  self.bound <- private$.bound

                  if (lower <= private$.lower) {
                    bound[1] <- self.bound[1]
                    lower <- private$.lower
                  }
                  if (upper >= private$.upper) {
                    bound[2] <- self.bound[2]
                    upper <- private$.upper
                  }

                  antibound <- character(0)
                  antibound[1] <- if (bound[1] == 'inf') 'max' else 'sup'
                  antibound[2] <- if (bound[2] == 'sup')'min' else 'inf'
                  new.1 <- new.2 <- NULL
                  if (lower != private$.lower || (self.bound[1] != 'inf' && antibound[1] != 'sup'))
                    new.1 <-
                    AtomicNumRange$new(private$.lower, lower, bound = c(self.bound[1], antibound[1]))
                  if (upper != private$.upper || (self.bound[2] != 'sup' && antibound[2] != 'inf'))
                    new.2 <-
                    AtomicNumRange$new(upper, private$.upper, bound = c(antibound[2], self.bound[2]))

                  if (length(new.1) && length(new.2)) return(NumRange$new(list = c(new.1, new.2)))
                  else if (length(new.1)) return(NumRange$new(list = new.1, simplify = FALSE))
                  else if (length(new.2)) return(NumRange$new(list = new.2, simplify = FALSE))
                },
                is_EmptyRange = function(){
                  is.null(private$.lower) && is.null(private$.upper)
                }
              ),
              private = list(
                #props
                .lower = numeric(),
                .upper = numeric(),
                .bound = character(),

                #methods
                .match_bound = function(bound){
                  c(c('min','inf')[c('min', 'inf') %in% bound],
                    c('max','sup')[c('max', 'sup') %in% bound])
                },
                .valid_bound = function(bound){
                  l <- length(bound)
                  if (l>2 | l<1) return(FALSE)
                  if (l==1) return(bound %in% c('inclusive', 'exclusive'))
                  return(
                    length(intersect(bound, c('inf', 'min'))) &
                      length(intersect(bound, c('sup', 'max')))
                  )
                },
                .valid_check = function(lower, upper, bound){
                  if (!is.numeric(lower)) stop('lower should be numeric!')
                  if (!is.numeric(upper)) stop('upper should be numeric!')
                  if (lower > upper) stop('Invalid range!')
                  valid.bound <- private$.valid_bound(bound)

                  if(!valid.bound) stop('Invalid boundary bound!')
                  invisible(TRUE)
                },
                .parse_formal = function(lower, upper, bound){
                  private$.valid_check(lower, upper, bound)
                  if (length(bound) == 1){
                    bound <-
                      if(bound == 'inclusive')
                        c('min','max')
                    else c('inf', 'sup')
                  } else
                    bound <- private$.match_bound(bound)

                  if (!identical(bound, c('min', 'max')) & lower == upper){
                    prefix <- if (bound[1] == 'min') '[' else '('
                    suffix <- if (bound[2] == 'max') ']' else ')'
                    stop('Invalid range:', prefix, lower, ',', upper, suffix)
                  }

                  if (is.infinite(upper)) bound[2] <- 'sup'
                  if (is.infinite(lower)) bound[1] <- 'inf'

                  return(list(lower = lower,
                              upper = upper,
                              bound = bound))
                },
                .parse_str = function(strObj){
                  prefix <- substr(strObj, 1, 1)
                  suffix <- substr(strObj, nchar(strObj), nchar(strObj))

                  bound <- character()

                  if (prefix == '[') bound[1] <- 'min'
                  else if (prefix == '(') bound[1] <- 'inf'
                  else stop('Invalid prefix:', prefix)

                  if (suffix == ']') bound[2] <- 'max'
                  else if (suffix == ')') bound[2] <- 'sup'
                  else stop('Invalid suffix:', suffix)

                  num <- substr(strObj, 2, nchar(strObj)-1)
                  num.split <- strsplit(num, '\\s?[,;]\\s?', perl=TRUE)[[1]]
                  lower <- as.numeric(num.split[1])
                  upper <- as.numeric(num.split[2])

                  return(private$.parse_formal(lower=lower, upper=upper, bound=bound))
                },
                .include_numeric = function(x){
                  if (is.na(x)) return(NA)
                  if (is.null(x)) return(logical(0))
                  lower.ok <- if ('min' %in% private$.bound | 'collapsed' %in% private$.bound)
                    x >= private$.lower else x > private$.lower
                  upper.ok <- if ('max' %in% private$.bound | 'collapsed' %in% private$.bound)
                    x <= private$.upper else x < private$.upper

                  return(lower.ok & upper.ok)
                },
                .include_range = function(range){
                  if (range$is_EmptyRange()) return(TRUE)
                  intersectRange <- self$intersect(range)
                  intersectRange$identical(range)
                }
              ),
              active = list(
                lower = function() return(private$.lower),
                upper = function() return(private$.upper),
                bound = function() return(private$.bound)
              ))

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cNumRange.R
\docType{data}
\name{NumRange}
\alias{NumRange}
\title{R6 Class Generator for Continuous Numeric Range}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
NumRange
 $new(..., list)
 $clone()
 $include(x)
 $cover(range)
 $covered_by(range)
 $cut(..., mode)
 $extract(id)
 $intersect_with(range)
 $union_with(range)
 $antiunion_with(range)
 $print()
}
\arguments{
\item{...}{For method 'new': spreaded list of NumRange.
For method 'cut': spreaded list of split points}

\item{id}{the number of element NumRange within the object. If left missing, the method will return all elements.}

\item{list}{A list of NumRange. Used in case NumRanges are in a form of list.}

\item{mode}{Mode for returned cutted NumRanges, if missing, defaults of element NumRanges are used.}

\item{range}{A range that will be deleted from the host range, a valid character input can also be converted into NumRange, for convenience.}

\item{x}{A numeric vector to check for inclusion.}
}
\value{
An object of R6Class "NumRange"
}
\description{
An R6 object for combined continuous numeric ranges
}
\details{
This function create a numeric range, via which you can check if a specific number is included or not.

After creation, you can use some built-in methods.

- $include will check if some provided numbers are included inside a range.

- $arrrange will create a sequence for the range with a definded step.

- $cut will cut the range into small range at the cutting point.

- $print will print the range in a beautified way.

- $union_with, $antiunion_with, and $intersect_with do union, anti_union, and intersect with a range.

- $cover and $covered_by: a range is covered by another if all of it elements are included in the later.

Note that $new, $include, $print(raw = FALSE) will accept method chaining.
}
\keyword{datasets}

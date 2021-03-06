% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_students.R
\name{fix_first_arg_in_fun}
\alias{fix_first_arg_in_fun}
\title{Change the first argument of a function}
\usage{
fix_first_arg_in_fun(fun, first_arg = "x")
}
\arguments{
\item{fun}{the function to change}

\item{first_arg}{the name of the first argument of the function to return, Default: x}
}
\value{
The original function, just with a different arg.
}
\description{
Useful when the teacher uses a function like function(x) and the student
does something like function(X) or function(y)
If the student had the first argument correct, it would not be changed.
}
\examples{
fo <- function(y, ...) {
  x+3
}
# fo(x=5) # errors...
fo_x <- fix_first_arg_in_fun(fo, "x")
fo_x(x=5)
}

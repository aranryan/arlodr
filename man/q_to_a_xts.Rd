% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ar_conversion.R
\name{q_to_a_xts}
\alias{q_to_a_xts}
\title{takes a quarterly xts object with multiple columns and converts to annual}
\usage{
q_to_a_xts(x, type)
}
\arguments{
\item{type}{}
}
\value{

}
\description{
need to give the type of conversion as argument, for example could do b <-
q_to_a_xts(suma, type="sum") this function uses the q_to_a function that I've
defined above, but then applys it to all of the columns of a xts object.
To convert using an average, set type="mean".
}

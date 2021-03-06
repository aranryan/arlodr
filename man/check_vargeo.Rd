% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_geo.R
\name{check_vargeo}
\alias{check_vargeo}
\title{check whether all intended geographies are
included in a dataframe}
\usage{
check_vargeo(x, check_for)
}
\arguments{
\item{check_for}{}
}
\value{

}
\description{
function to check whether all intended geographies are
included in a dataframe of series set up with var_geo
format series names
input a dataframe that is set up for eviews, for example,
with series names as column headers and dates in a date column
arguments: check_for = vector of codes to check for, such as based on
geo_for_eviews
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_state_name.R
\name{create_state_name_df}
\alias{create_state_name_df}
\title{Create state name dataframe}
\usage{
create_state_name_df(x)
}
\arguments{
\item{x}{}
}
\value{

}
\description{
Creates a dataframe with a row for each state. Provides:
cen_region Census Region
cen_division Census Division
state_name State name written out
state_abb Two-letter state code in upper case
}
\details{
The dataframe has 51 rows because DC has been appended.
Also, the Middle Atlantic Division is referred to as Mid-Atlantic, and
Midwest is used to replace the former name of the North Central Census
region.
}
\examples{
state_name <- create_state_name_df()

}

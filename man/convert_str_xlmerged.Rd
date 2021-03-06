% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_str.R
\name{convert_str_xlmerged}
\alias{convert_str_xlmerged}
\title{Convert str data from Excel with merged column headers to tidy csv}
\usage{
convert_str_xlmerged(input_file_name, output_file_name, input_path)
}
\arguments{
\item{input_file_name}{Name of the input file.}

\item{output_file_name}{Name of the output file.}

\item{input_path}{}
}
\value{

}
\description{
Function that can import the US or WW data file from STR.
This has to deal with the fact that the file has merged cells for the market names.
}
\examples{

arlodr::convert_str_xlmerged(
input_file_name = "TourismEconomicsUS_201708.xls",
output_file_name = "strtidy_us_201708.csv",
input_path = c("input_data/str_data_restrict/"))

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_d_index.R
\name{ca_d_index}
\alias{ca_d_index}
\title{Calculate the Index of dissimilarity (D-index) of CA}
\usage{
ca_d_index(CA, zone, CA_exclude_value = 0, with_zone_level = FALSE)
}
\arguments{
\item{CA}{A matrix/array or a list of them representing the cellular automaton wherein each entry is coded with the group attribute.}

\item{zone}{A matrix/array or a list of them representing the zones.}

\item{CA_exclude_value}{Values in CA to be excluded from the calculation (default is \code{0}).}

\item{with_zone_level}{Logical, if \code{TRUE}, the function returns D index for each zone in addition to the global D-index (default is \code{FALSE}).}
}
\value{
If \code{with_zone_level} is \code{TRUE}, a list with two elements:
\describe{
\item{d}{The D index.}
\item{d_by_zone}{The D index by zone.}
}
Otherwise, returns the D index.
}
\description{
This function calculates the D-index,
which is a measure of segregation given cross-tabulated data for multiple zones and groups.
}
\examples{
\dontrun{
d <- ca_d_index(CA = agrD$room_by_ethnicity, zone = agrD$zone, with_zone_level = TRUE)
d$d_by_zone
}

}

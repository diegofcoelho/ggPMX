% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pmx-filter.R
\name{pmx_filter}
\alias{pmx_filter}
\title{filter data in a pmx controller}
\usage{
pmx_filter(ctr, data_set = c("estimates", "predictions", "eta",
  "finegrid", "shrink", "input", "individual"), pmx_exp)
}
\arguments{
\item{ctr}{A controller. An object of 'pmxClass'}

\item{data_set}{A data_set within the controller to apply a filter to.}

\item{pmx_exp}{A filter expression}
}
\value{
Returns a pmx controller with a filtered data set.
}
\description{
filter data in a pmx controller
}
\examples{
\donttest{
## example of global filter
ctr <- theophylline()
ctr \%>\% pmx_filter(data_set = "prediction", ID == 5 & TIME < 2)
ctr \%>\% get_data("prediction")
}
}

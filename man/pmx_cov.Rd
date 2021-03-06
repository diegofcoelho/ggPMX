% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-eta-cov.R
\name{pmx_cov}
\alias{pmx_cov}
\title{Select/Map covariates using human labels}
\usage{
pmx_cov(values, labels = NULL)
}
\arguments{
\item{values}{\code{list} of covariates to use to  create the plot}

\item{labels}{\code{list} of covariates facets labels}
}
\value{
\code{pmxCOVObject} object
}
\description{
Select/Map covariates using human labels
}
\details{
In case of `pmx_plot_eta_cats` and `pmx_plot_eta_conts` you can customize the covariates
and covaraites labels using `pmx_cov`.
}

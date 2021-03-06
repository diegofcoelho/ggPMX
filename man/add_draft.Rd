% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_draft}
\alias{add_draft}
\title{Add draft layer annotation}
\usage{
add_draft(label = "DRAFT", size = 10, colour = "grey50", x = Inf,
  y = -Inf, ...)
}
\arguments{
\item{label}{draft layer default to DRAFT}

\item{size}{size of the annotation}

\item{colour}{color of the annotation default to grey50}

\item{x}{\code{numeric} x coordinate of the draft label}

\item{y}{\code{numeric} y coordinate of the draft label}

\item{...}{extra parameters to geom text used to annotate the draft}
}
\value{
ggplot2 annotation
}
\description{
This function adds the word draft to certain graphics.
}

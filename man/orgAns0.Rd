% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org2tree.R
\name{orgAns0}
\alias{orgAns0}
\title{Utility function for \code{org2LoL}}
\usage{
orgAns0(inp, lvl, LVLS)
}
\arguments{
\item{inp}{Input}

\item{lvl}{Level}

\item{LVLS}{Levels}
}
\value{
A \code{list} of \code{list}s
}
\description{
This does the work underlying \code{org2LoL} and \code{org2tree}.
The function recursively evaluates to parse Emacs org-mode files.
}
\author{
Pete Dodd
}

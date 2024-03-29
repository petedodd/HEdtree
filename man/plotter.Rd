% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotter.R
\name{plotter}
\alias{plotter}
\title{A plotter for decision tree models}
\usage{
plotter(node, varz = c("name", "cost", "qol"), edgelabel = TRUE)
}
\arguments{
\item{node}{a \code{data.tree} object that must contain a probability field \code{p}}

\item{varz}{Variables}

\item{edgelabel}{Edge labels}
}
\value{
\code{DiagrammeR} object
}
\description{
This is a simplified plotting utility that allows visual checking of decision
tree models. By default, the node names, costs, HRQoLs are displayed as well
as the edge probabilities. The node attributes to display can be specified in
\code{varz}. The boolean \code{edgelabel} variable allows turning the edge labels off.
}
\author{
Pete Dodd
}

##' Labelling utilities
##' 
##' These are two labelling functions to label edges
##' and nodes used by \code{plotter}.
##'
##' @template args-node
##' @param varz Variables
##' @return \code{string} to be used as label
##' @author Pete Dodd
GetNodeLabel <- function(node,varz=c('name','cost','qol')){
  nmz <- c()
  for(nm in varz) nmz <- c(nmz,node[[nm]])
  paste0(nmz,collapse = "\n")
}

#'
GetEdgeLabel <- function(node,var='p') node[[var]]

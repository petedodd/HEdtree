##' A plotter for decision tree models
##' 
##' This is a simplified plotting utility that allows visual checking of decision
##' tree models. By default, the node names, costs, HRQoLs are displayed as well
##' as the edge probabilities. The node attributes to display can be specified in
##' `varz`. The boolean `edgelabel` variable allows turning the edge labels off.
##'
##' @template args-node
##' @param varz Variables 
##' @param edgelabel Edge labels
##' @return \code{DiagrammeR} object
##' @author Pete Dodd
##' @importFrom grDevices dev.off pdf png
##' @importFrom graphics curve title
##' @importFrom stats dbeta dexp dgamma dlnorm dnorm
##'             qbeta qexp qgamma qlnorm qnorm rbeta
##'             rexp rgamma rlnorm rmultinom rnorm runif
##' @importFrom utils write.csv
##' @import ggplot2
##' @md
##' @export
plotter <- function(node, varz=c('name','cost','qol'), edgelabel = TRUE) {
  
  nodelab <- function(x) GetNodeLabel(x, varz=varz)
  
  if (edgelabel) {
    SetEdgeStyle(node, fontname = 'helvetica', label = GetEdgeLabel)
  } else {
    SetEdgeStyle(node, fontname = 'helvetica', label = '')
  }
  SetNodeStyle(node, fontname = 'helvetica', label = nodelab, shape = 'box')
  SetGraphStyle(node, rankdir = "LR")
  plot(node)
}

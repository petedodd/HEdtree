##' This is a simplified plotting utility that allows visual checking of decision tree models. By default, the node names, costs, HRQoLs are displayed as well as the edge probabilities. The node attributes to display can be specified in `varz`. The boolean `edgelabel` variable allows turning the edge labels off.
##'
##' @title A plotter for decition tree models
##' @param node 
##' @param varz 
##' @param edgelabel 
##' @return \code{DiagrammeR} object
##' @author Pete Dodd
##' @export
plotter <- function(node,varz=c('name','cost','qol'),edgelabel = TRUE ){
  nodelab <- function(x) GetNodeLabel(x,varz=varz)
  if(edgelabel){
    SetEdgeStyle(node, fontname = 'helvetica', label = GetEdgeLabel)
  } else {
    SetEdgeStyle(node, fontname = 'helvetica', label = '')
  }
  SetNodeStyle(node, fontname = 'helvetica', label = nodelab, shape = 'box')
  SetGraphStyle(node, rankdir = "LR")
  plot(node)
}

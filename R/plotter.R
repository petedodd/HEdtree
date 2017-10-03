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

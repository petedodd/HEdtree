GetNodeLabel <- function(node,varz=c('name','cost','qol')){
  nmz <- c()
  for(nm in varz) nmz <- c(nmz,node[[nm]])
  paste0(nmz,collapse = "\n")
}
GetEdgeLabel <- function(node,var='p') node[[var]]

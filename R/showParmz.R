##' This function just fetches the parameters used in the model (and prints them). It separates those which are basic and require separate definition from those that are specified via calculations.
##'
##' @title List parameters in model
##' @param node 
##' @param parmz 
##' @return \code{list} of length 2: unique basic parameters & unique calculation-defined parameters
##' @author Pete Dodd
##' @export
showParmz <- function(node,parmz=c('cost','p','qol')){
  varnmz <- c()
  for(pm in parmz)
    varnmz <- c(varnmz,node$Get(pm))
  varnmz <- varnmz[!is.na(varnmz)]
  calx <- grepl("[*|(|)|+|/|-]",varnmz) #which are calculations
  calcs <- varnmz[calx]                 #calculations
  varnmz <- varnmz[!calx]               #atomic
  print(varnmz)
  varnmz <- unique(varnmz)
  calcs <- unique(calcs)
  list(vars=varnmz,calcs=calcs)
}

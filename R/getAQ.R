##' Get an arbitrary string for tree calculations
##' 
##' This can be used to generate a string corresponding to calculating
##' the mean tree output for a given quantity over the tree.
##' The tree is specified as the input \code{node}, which must be a
##' \code{data.tree} object containing probability variables as \code{p}.
##' The quantity to be averaged over must be a node attribute \code{qnt},
##' which defaults as \code{cost}.
##' 
##' @template args-node
##' @param qnt 
##' @return \code{string} to be evaluated in calculations
##' @author Pete Dodd
##' @export
getAQ <- function(node,qnt='cost') {
    ss <- node[[qnt]]
    if(!node$isRoot & is.null(node$p))
      stop(paste0('Probability not found for node:', node$name,'!'))
    if(is.null(ss)) ss <- '0'
    if( node$leafCount > 0 ){
        for( kid in node$children ){
            ss <- paste0(ss,'+(',kid$p,')*(',getAQ(kid,qnt=qnt),')')
        }
    }
    ss
}

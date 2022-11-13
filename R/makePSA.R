##' Make PSA data.table from parameter object
##' 
##' This function facilitates generating a PSA data.table object from a parameter object
##'
##' @param N number of samples (rows)
##' @param P parameter object (eg from parse.parmtable)
##' @param dbls list of 2-D name vectors for any parameter pairs
##'    generated jointly (same order as in \code{P})
##' @return \code{data.table}
##' @author Pete Dodd
##' @import data.table
##' @export
makePSA <- function(N,P,dbls=list()){
    D <- nmz <- list()
    nd <- 1
    for (nm in names(P)) {
        if(length(P[[nm]])>1){
            D[[nm]] <- data.table::data.table(P[[nm]]$r(N))
        } else { #non-random parameters
            D[[nm]] <- data.table::data.table(rep(P[[nm]],N))
        }
        if (ncol(D[[nm]]) == 2) {
            if (nd <= length(dbls)) {
                nmz[[nm]] <- colnames(D[[nm]]) <- dbls[[nd]]
                nd <- nd + 1
            }
        }
        else {
            names(D[[nm]]) <- nm
            nmz[[nm]] <- nm
        }
    }
    D <- data.table::setDT(unname(unlist(D, recursive = FALSE)))
    nmz <- unname(unlist(nmz))
    names(D) <- nmz
    return(D)
}
## TODO could extend this to LHS sampling

##' This function is designed to simplify the workflow by allowing specification of parameters/priors in an external CSV file following certain conventions.
##'
##' Work in progress: need to add other distributions; testing functionality etc
##'
##' @title Reading a parameter table and making utility functions
##' @param data 
##' @param outfile 
##' @param dpl 
##' @param testdir 
##' @return \code{list} of functions for RV computations and associated names
##' @author Pete Dodd
##' @export
parse.parmtable <- function(data,outfile=NULL,dpl=3,testdir=NULL){
    parz <- as.character(data[,1])
    ds <- as.character(data[,2])
    ## process ds
    sp <- strsplit(ds,'\\(')
    dss <- unlist(lapply(sp,FUN = function(x)x[1] )) #distributions
    dp <- lapply(sp,FUN = function(x)x[2] ) #parms
    dp <- unlist(lapply(dp,FUN=function(X)strsplit(X,"\\)")))
    com <- grepl(',',dp)                #commas
    rez <- list()
    for(i in seq_along(parz)){
        if(com[i]){
            pz <- as.numeric(unlist(strsplit(dp[i],",")))
            rd <- switch(dss[i],
                         LN = function(n)rlnorm(n,pz[1],pz[2]),
                         N = function(n)rnorm(n,pz[1],pz[2]))
            dd <- switch(dss[i],
                         LN = function(x)dlnorm(x,pz[1],pz[2]),
                         N = function(x)dnorm(x,pz[1],pz[2]))
            qd <- switch(dss[i],
                         LN = function(x)qlnorm(x,pz[1],pz[2]),
                         N = function(x)qnorm(x,pz[1],pz[2]))
        } else {
            cat('not done yet!\n')            
        }
        rez[[parz[i]]] <- list(d=dd,r=rd,q=qd,name=dss[i])
    }
    rez
}

## TODO: tests, other items etc
## tst <- parse.parmtable(data.frame(parm=c('blah','nla'),dist=c('LN(2,3)','N(2,3)')))
## tst[[1]]$r(10)
## tst[['blah']]$r(10)

##' This function is designed to simplify the workflow by allowing specification of parameters/priors in an external CSV file following certain conventions.
##'
##' Work in progress: need to add other distributions; testing functionality etc
##' The following distributions are handled:
##'
##' Normal: defined as N(mean,sd)
##' Log-Normal: defined as LN(mu,sdlog)
##' Beta: defined as B(a,b)
##' 
##' TODO document & expand inputs below,
##'
##' @title Reading a parameter table and making utility functions
##' @param data 
##' @param outfile 
##' @param dpl 
##' @param testdir 
##' @return \code{list} of functions for RV computations and associated names
##' @author Pete Dodd
##' @export
parse.parmtable <- function(data,       #in data, first col parm name, second defn
                            outfile=NULL, #name for test table out
                            dpl=3,        #number of dps for reporting (TODO)
                            testdir=NULL, #test directory & flag TODO
                            qn=.75        #quantiles to report in table TODO
                            ){
    parz <- as.character(data[,1])
    ds <- as.character(data[,2])
    info <- matrix(nrow=nrow(data),ncol=2)
    if(ncol(data)>3)                    #any extra info stored in case useful
      info <- apply(data[,3:ncol(data)],2,as.character)
    ## process ds
    com <- grepl(',',ds)                #commas
    sp <- strsplit(ds,'\\(')
    dss <- unlist(lapply(sp,FUN = function(x)x[1] )) #distributions
    dp <- lapply(sp,FUN = function(x)x[2] )      #parms
    dp <- unlist(lapply(dp,FUN=function(X)strsplit(X,"\\)")))
    rez <- list()
    for(i in seq_along(parz)){
        if(com[i]){
            pz <- as.numeric(unlist(strsplit(dp[i],",")))
            rd <- switch(dss[i],
                         LN = function(n)rlnorm(n,pz[1],pz[2]),
                         N = function(n)rnorm(n,pz[1],pz[2]),
                         B = function(n)rbeta(n,pz[1],pz[2]))
            dd <- switch(dss[i],
                         LN = function(x)dlnorm(x,pz[1],pz[2]),
                         N = function(x)dnorm(x,pz[1],pz[2]),
                         B = function(n)qbeta(n,pz[1],pz[2]))
            qd <- switch(dss[i],
                         LN = function(x)qlnorm(x,pz[1],pz[2]),
                         N = function(x)qnorm(x,pz[1],pz[2]),
                         B = function(n)qbeta(n,pz[1],pz[2]))
            rez[[parz[i]]] <- list(d=dd,r=rd,q=qd,name=dss[i],info=info[i,])
        } else {
          rez[[parz[i]]] <- list(value=as.numeric(ds[i]),info=info[i,])
        }
    }
    rez
}

## TODO: tests, other items etc
## MVN
## tst <- parse.parmtable(data.frame(parm=c('blah','nla'),dist=c('LN(2,3)','N(2,3)')))
## tst[[1]]$r(10)
## tst[['blah']]$r(10)

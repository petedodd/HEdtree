##' This function is designed to simplify the workflow by allowing specification of parameters/priors in an external CSV file following certain conventions.
##'
##' Work in progress: need to add other distributions; testing functionality etc
##' @details
##' The following distributions are handled:
##'
##' \itemize{
##'  \item{"Normal"}{defined as \code{N(mean,sd)}}
##'  \item{"Log-Normal"}{defined as \code{LN(mu,sdlog)}}
##'  \item{"Beta"}{defined as \code{B(a,b)}}
##' }
##'
##' TODO document & expand inputs below,
##'
##' @title Reading a parameter table and making utility functions
##' @param data in data, first col parm name, second definition
##' @param outfile optional name for test table out
##' @param dpl number of dps for reporting (TODO)
##' @param testdir test directory. Settings this acts as a flag for action. TODO
##' @param qn quantiles to report in table TODO
##' @return \code{list} of functions for RV computations and associated names
##' @examples
##' tst <- parse.parmtable(data.frame(parm=c('blah','nla'),dist=c('LN(2,3)','N(2,3)')))
##' tst[[1]]$r(10)
##' tst[['blah']]$r(10)
##'
##' @author Pete Dodd
##' @import stringr
##' @import MASS
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
    ## if(ncol(data)>3)                    #any extra info stored in case useful
    ##   info <- apply(data[,3:ncol(data)],2,as.character)
    ## process ds
    com <- grepl(',',ds)                #commas
    mvn <- grepl('MVN',ds)              #MVN
    colons <- grepl(':',parz)           #colons
    vecs <- stringr::str_count(ds,"\\[") > 0 #vectors
    mats <- stringr::str_count(ds,"\\[") > 1 #matrices
    sp <- strsplit(ds,'\\(')
    dss <- unlist(lapply(sp,FUN = function(x)x[1] )) #distributions
    dp <- lapply(sp,FUN = function(x)x[2] )      #parms
    dp <- unlist(lapply(dp,FUN=function(X)strsplit(X,"\\)")))
    rez <- list()
    for(i in seq_along(parz)){
      if(com[i] & !mats[i] & !colons[i] & !vecs[i]){
        ne <- new.env()       #to avoid functions associating with wrong data
        ne$parm <- parz[i]
        ne$pz <- as.numeric(unlist(strsplit(dp[i],",")))
        ne$name <- dss[i]
        ne$r <- switch(dss[i],
                     LN = function(n)rlnorm(n,pz[1],pz[2]),
                     N = function(n)rnorm(n,pz[1],pz[2]),
                     B = function(n)rbeta(n,pz[1],pz[2]))
        ne$d <- switch(dss[i],
                     LN = function(x)dlnorm(x,pz[1],pz[2]),
                     N = function(x)dnorm(x,pz[1],pz[2]),
                     B = function(x)dbeta(x,pz[1],pz[2]))
        ne$q <- switch(dss[i],
                     LN = function(x)qlnorm(x,pz[1],pz[2]),
                     N = function(x)qnorm(x,pz[1],pz[2]),
                     B = function(x)qbeta(x,pz[1],pz[2]))
        environment(ne$r) <- ne
        environment(ne$d) <- ne
        environment(ne$q) <- ne
        rez[[parz[i]]] <- as.list(ne)   #bit more user-friendly
      } else {
        ## not just simple distribution
        pn <- as.character(parz[i]); sn <- ''
        if(colons[i]){
          tmp <- unlist(strsplit(pn,split=":")) #first and second bits
          tmp <- gsub(" ","",tmp)     #ditch whitespace
          pn <- tmp[1]; sn <- tmp[2]
          if(! pn %in% names(rez)) rez[[pn]] <- list()
        }
        if(mvn[i]) {                  #MVN distribution!
          ds[i] <- gsub('MVN:','',ds[i])
        }
        if(mats[i]){                              #matrix
          nr <- stringr::str_count(ds[i],"\\[")-1    #number of rows (assume sqr!)
          ds[i] <- gsub("\\]\\s*,\\s*\\[",",",ds[i]) #gets rid of internal "],["
          ds[i] <- gsub("\\[\\s*\\[","matrix(c(",ds[i])
          ds[i] <- gsub("\\]\\s*\\]",
                        paste0("),nrow=",nr,",ncol=",nr,",byrow=TRUE)"),ds[i])
          ds[i] <- gsub("\\]",paste0("),nrow=",nr,",ncol=",nr,",byrow=TRUE)"),ds[i])
          M <- eval(parse(text=ds[i]))
          if(colons[i]){               #if/else
            rez[[pn]][[sn]] <- M
          } else{
            rez[[pn]] <- M
          }
        }
        if(vecs[i] & !mats[i]){                  #vector
          ds[i] <- gsub("\\[","c(",ds[i])
          ds[i] <- gsub("\\]",")",ds[i])
          V <- eval(parse(text=ds[i]))
          if(colons[i]){               #if/else
            rez[[pn]][[sn]] <- V
          } else{
            rez[[pn]] <- V
          }
        }
        if(mvn[i] & length(rez[[pn]])==2) {#MVN distribution ready
          ## TODO pick out mean and var based on nrow
          rez[[pn]][['name']] <- 'MVN'
          rez[[pn]]$Uchol <- chol(rez[[pn]]$sg)
          rez[[pn]]$parm <- pn
          ne <- list2env(x=rez[[pn]])
          ne$r <- function(n) MASS::mvrnorm(n,mu=mn,Sigma=sg) #TODO - check
          ne$d <- function(x) 0
          ne$q <- function(x){
            z <- qnorm(x)
            z <- z %*% Uchol      #make these correlated normals 
            z + matrix(mn,ncol=ncol(z),nrow=nrow(z),byrow=TRUE)
          }          #TODO check
          environment(ne$r) <- ne
          environment(ne$d) <- ne
          environment(ne$q) <- ne
          rez[[pn]] <- as.list(ne)
        }
        ## conditions for scalar
        if(!vecs[i] & !mats[i] & !mvn[i]){
          V <- as.numeric(ds[i])
          if(colons[i]){               #if/else
            rez[[pn]][[sn]] <- V
          } else{
            rez[[pn]] <- V
          }
        }
      }
    }
    if(!is.null(testdir)){
      if(is.null(outfile)) outfile <- paste0(testdir,'/out_parmtable.csv')
      ## output tests! TODO
    }
    rez
}

## TODO: tests, other items, vignette etc


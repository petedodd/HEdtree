##' List parameters in model
##' 
##' This function just fetches the parameters used in the model (and prints them).
##' It separates those which are basic and require separate definition from
##' those that are specified via calculations.
##'
##' @template args-node
##' @param parmz Parameters
##' @return \code{list} of length 2: unique basic parameters
##'  and unique calculation-defined parameters
##' @author Pete Dodd
##' @export
showParmz <- function(node,parmz=c('cost','p','qol')){
  varnmz <- c()
  for (pm in parmz)
    varnmz <- c(varnmz,node$Get(pm))
  varnmz <- varnmz[!is.na(varnmz)]
  calx <- grepl("[*|(|)|+|/|-]",varnmz) #which are calculations
  calcs <- varnmz[calx]                 #calculations
  varnmz <- varnmz[!calx]               #atomic
  varnmz <- varnmz[varnmz!='0']
  varnmz <- varnmz[varnmz!='1']
  ## print(varnmz)
  varnmz <- unique(varnmz)
  calcs <- unique(calcs)
  list(vars = varnmz,
       calcs = calcs)
}


##' List parameters in model
##' 
##' Fetches the parameters used in the model for use in testing.
##'
##' @param TREE decision tree object
##' @return \code{vector} of  unique basic parameters
##' @author Pete Dodd
##' @export
showAllParmz <- function(TREE){
  B <- showParmz(TREE)
  ## get calx
  cx <- B$calcs
  cx <- gsub("\\*|\\+|-|\\/|\\(|\\)"," ",cx)
  cx <- paste(cx,collapse=" ")
  cx <- strsplit(cx,split=" ")[[1]]
  cx <- cx[cx!=""]
  cx <- cx[cx!="1"]
  ## get non calcs
  cx <- c(cx, B$vars)
  unique(cx)
}

##' List parameters in model
##' 
##' This function makes a dummy PSA dataset based on a set of
##' parameters for function testing
##'
##' @param ncheck Number to check
##' @param vnames Vector of names
##' @return \code{data.frame} of dummy PSA for testing tree functions
##' @author Pete Dodd
##' @export
makeTestData <- function(ncheck,vnames){
  A <- data.table::data.table(vnames,value=runif(length(vnames)))
  A <- A[rep(1:length(vnames),each=ncheck)]
  idz <- rep(1:ncheck,length(vnames))
  A$id <- idz
  A$value <- runif(nrow(A))
  data.table::dcast(A,id~vnames,
                    value.var = 'value')
}
## TODO document risks etc - runifs
## ensure that data.table dependency supplied


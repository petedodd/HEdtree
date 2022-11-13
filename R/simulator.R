
##' Simulate numbers on the tree
##' 
##' This is for simulating numbers from studies.
##' The tree can be converted afterwards with something like
##'
##' \code{ToDataFrameTable(RStbtxo, "pathString", "N")}
##'
##' which is part of the data.tree library.
##'
##' @param tree input tree
##' @param N integer inflow
##' @param P list of data.frame/table row of parameters
##' @return by side-effect on tree as N
##' @author Pete Dodd
##' @export
simulate <- function(tree,              #tree
                     N,                 #inflow
                     P){                #parameters
  tree$N <- N
  nk <- length(tree$children)
  if(nk>0){
    pz <- rep(0,nk)
    for(i in 1:nk){
      txt <- tree$children[[i]]$p
      pz[i] <- eval(parse(text=txt),envir=P)
    }
    Nz <- c(rmultinom(1,N,pz))          #how many flow into each branch
    for(i in 1:nk){
      simulate(tree$children[[i]],Nz[i],P)
    }
  }
}


#'
runningprobs0 <- function(tree,rp){
  txt <- paste0("(",rp,")*(",tree$p,")")
  tree$runningprob <- txt
  nk <- length(tree$children)
  if(nk>0){
    for(i in 1:nk){
      runningprobs0(tree$children[[i]],txt)
    }
  }
}

##' Attach probabilities of reaching each node
##' 
##' @param tree input tree
##' @return by side-effect on tree as runningprob
##' @author Pete Dodd
##' @export
runningprobs <- function(tree){
  tree$runningprob <- '1'
  nk <- length(tree$children)
  if(nk>0){
    for(i in 1:nk){
      runningprobs0(tree$children[[i]],1)
    }
  }
}

##' Parameters for a lognormal distribution: mu & sdlog from median/mean and variance
##'
##' @title Get parameters for a log-normal distribution
##' @param mid the midpoint of the distribution (median/mean depending on \code{med})
##' @param var the variance of the distribution
##' @param med logical to assume \code{mid} is median (default) or otherwise mean.
##' @return \code{list} mu, sig being mu and sdlog for lognormal distribution
##' @author Pete Dodd
##' @export
getLNparms <- function(mid,var,med=TRUE){
  if(med){
    mu <- log(mid)                      #mid as median
    x <- (1+sqrt(1+4*var/mid^2))/2
    ans <- list(mu=mu,sig=sqrt(log(x)))
  } else {
    s2 <- log(1+var/mid^2)
    mu <- log(mid) - s2/2
    ans <- list(mu=mu,sig=sqrt(s2))
  }
  ans
}

##' Parameters for a beta distribution: a & b from 
##'
##' @title Get parameters for a beta distribution
##' @param E the mean
##' @param V the variance 
##' @return \code{list} containing a & b
##' @author Pete Dodd
##' @export
getAB <- function(E,V){
    ## V <- (U-L)/4 #variance for uppers and lower
    sz <- E*(1-E)/V - 1
    a <- sz*E
    b <- sz*(1-E)
    return(list(a=a,b=b))
}

##' Utility function to trim whitespace
##'
##' @title Trim whitespace
##' @param x 
##' @return string 
##' @author Pete Dodd
##' @export
trm <- function(x)gsub(" ","",x)

##' Utility function for extracting midpoint from bracketed uncertainty ranges
##'
##' Expects something like M (Mlo,Mhi) or M (Mlo - Mhi) 
##' @title Get midpoint from CIs
##' @param x 
##' @return numeric
##' @author Pete Dodd
##' @export
midpnt <- function(x){
  x <- gsub("(.?)\\(.*","\\1",x,perl=TRUE)
  as.numeric(trm(x))
}

##' Utility function for extracting low point from bracketed uncertainty ranges
##'
##' Expects something like M (Mlo,Mhi) or M (Mlo - Mhi) 
##' @title Get low point from CIs
##' @param x 
##' @return numeric 
##' @author Pete Dodd
##' @export
lopnt <- function(x){
  x <- gsub(".*\\((.*?)[,|-].*","\\1",x,perl=TRUE)
  as.numeric(trm(x))
}

##' Utility function for extracting high point from bracketed uncertainty ranges
##'
##' Expects something like M (Mlo,Mhi) or M (Mlo - Mhi) 
##' @title Get high point from CIs
##' @param x 
##' @return numeric
##' @author Pete Dodd
##' @export
hipnt <- function(x){
  x <- gsub(".*[,|-](.*?)\\).*","\\1",x,perl=TRUE)
  as.numeric(trm(x))
}

##' Logit function
##'
##' @title Logit function
##' @param x 
##' @return 
##' @author Pete Dodd
##' @export
logit <- function(x)log(x/(1-x))

##' Inverse logit function
##'
##' @title Inverse logit function
##' @param x 
##' @return 
##' @author Pete Dodd
##' @export
ilogit <- function(x)exp(x)/(1+exp(x))



## ======== tree manipulation utility functions ========

##' Merge a tree onto another by node name
##'
##' @title Merge a tree onto another by node name
##' @param rootnode 
##' @param nodetoadd 
##' @param nodename 
##' @param usecase Match case in nodename? (must be TRUE currently)
##' @param leavesonly Merge only onto leaves? 
##' @return NULL
##' @author Pete Dodd
##' @export
MergeByName <- function (rootnode,
                         nodetoadd,
                         nodename,
                         usecase = TRUE,
                         leavesonly = FALSE) {
    if(!leavesonly){
        rootnode$Do(function(node)
            for (K in nodetoadd$children) node$AddChildNode(Clone(K)),
            filterFun = function(x) (x$name == nodename) )
    } else {
        rootnode$Do(function(node)
            for (K in nodetoadd$children) node$AddChildNode(Clone(K)),
            filterFun = function(x) (x$name == nodename) && x$isLeaf )
    }
    ## by side-effect
}

##' Ditch the top of tree on reading
##'
##' @title Ditch the top of tree on reading
##' @param x 
##' @return 
##' @author Pete Dodd
##' @export
top <- function(x) x$children[[1]]

##' Ditch the top of tree on reading
##'
##' @title Ditch the top of tree on reading
##' @param tree tree to plot
##' @param fn file name
##' @return NULL
##' @author Pete Dodd
##' @export
savetreeplot <- function(tree,fn)
  DiagrammeR::export_graph(data.tree::ToDiagrammeRGraph(tree),file_name=fn)



##' Simpler text file to tree
##'
##' See also MSorg2tree
##' 
##' @title Make a tree from a tsv
##' @param x filename relative to 'here'
##' @return A tree
##' @author Pete Dodd
##' @import here
##' @export
txt2tree <- function(x) top(MSorg2tree(here::here(x)))

##' Write a CSV tree with labels
##'
##' @title Write a CSV tree with labels
##' @param TREE the tree
##' @param filename file to write to
##' @param ... label names to include
##' @author Pete Dodd
##' @export
tree2file <- function(TREE,filename,...){
    tmp <- data.tree::ToDataFrameTree(TREE,...)
    tmp <- data.table::as.data.table(tmp)
    data.table::fwrite(tmp,file=filename)
}


##' Append a number of results to data
##'
##' @title Run results
##' @param D the PSA data
##' @param L a list of functions
##' @param nmz an optional vector specifying a subset of the functions in \code{F} to run
##' @param verbose print what's happening (default=\code{TRUE})
##' @author Pete Dodd
##' @export
appendResults <- function(D,L,nmz=NULL,verbose=TRUE){
  ## if not using nmz to specify
  if(is.null(nmz)){
    nmz <- names(L)
    nmz <- gsub('fun$','',names(L))
    nmz <- nmz[nmz!='p'] #remove p if there
  }
  ## loop
  for(nm in nmz){
    if(verbose) cat('Calculating answers for: ',nm,'\n')
    fnm <- paste0(nm,'fun')
    D[[nm]] <- L[[fnm]](D)
  }
  if(verbose) cat('Done!\n')
  D
}

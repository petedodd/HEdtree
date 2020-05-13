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
##' @return 
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

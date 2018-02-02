##function for extracting lognormal parz from mid point and variance
getLNparms <- function(mid,var){        
    mu <- log(mid)                      #mid as median
    x <- (1+sqrt(1+4*var/mid^2))/2
    list(mu=mu,sig=sqrt(log(x)))
}

## s = E(1-E)/V - 1; a = sE, b=s(1-E); V = (u-l)/4
getAB <- function(E,L,U){
    V <- (U-L)/4
    sz <- E*(1-E)/V - 1
    a <- sz*E
    b <- sz*(1-E)
    return(list(a=a,b=b))
}

## TODO document & export

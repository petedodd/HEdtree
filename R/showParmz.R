showParmz <- function(node,parmz=c('cost','p','qol')){
  varnmz <- c()
  for(pm in parmz)
    varnmz <- c(varnmz,node$Get(pm))
  varnmz <- varnmz[!is.na(varnmz)]
  calx <- grepl("[*|(|)|+|/|-]",varnmz) #which are calculations
  calcs <- varnmz[calx]                 #calculations
  varnmz <- varnmz[!calx]               #atomic
  list(vars=varnmz,calcs=calcs)
}

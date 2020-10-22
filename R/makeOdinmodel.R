##' @title Making Odin ODEs from tree
##' @param tree input tree
##' @param rootname base for naming convention
##' @param rootinflow if adding a flow in from existing model
##' @param dimensions the dimensions if an array model
##' @param times.array should the timescales also be arrays or not?
##' @param quants vector of entry-accruing quantities like cost to accumulate
##' @return list
##' @author Pete Dodd
##' @export 
makeOdinModel <- function(tree,
                          rootname='N1',    #naming convention
                          rootinflow='0',   #specify a root inflow
                          dimensions=c(),   #dimensions of arrays
                          times.array=FALSE,#are times arrays as well?
                          quants=c()        #rates to compute
                          ){
  ## add aliased names: should work by side-effect on tree
  tree$Set(oname=rootname)
  tree$Do(function(node) if(!node$isLeaf){
                         for(i in 1:node$count)
                           node$children[[i]]$oname <- paste(node$oname,i,
                                                             sep='.')
                         })

  ## dimension prep stuff
  L <- length(dimensions)
  brkts <- fbrkts <- fbrktsT <- ''
  if(L>0){
    inmz <- c('i','j','k',paste0('i',1:5)) #TODO check
    ## bits for empty brackets
    if(L>1)
      brkts <- rep(',',L-1)
    brkts <- paste(brkts,collapse='')
    brkts <- paste0('[',brkts,']')
    ## bits for full brackets
    fbrkts <- inmz[1:L]
    fbrkts <- paste(fbrkts,collapse=',')
    fbrkts <- fbrktsT <- paste0('[',fbrkts,']')
    if(!times.array) fbrktsT <- ''
    ## dimension bits
    dmz <- paste(dimensions,collapse = ',')
    dmz <- paste0('c(',dmz,')')
  }

  ## grab parameters
  tmp <- showParmz(tree,parmz=c('p','T'))
  pmz <- tmp$vars #TODO this needs correcting no ltfu2.3
  pmz <- pmz[pmz!='0']
  pmz <- pmz[pmz!='1']
  tmp <- tmp$calcs
  tmp <- unique(unlist(strsplit(tmp,split="-|/|\\*|\\+"))) #find variables in calculations
  ditch <- grepl("^[0-9]+$", tmp, perl = TRUE)
  tmp <- tmp[!ditch]
  pmz <- unique(c(pmz,tmp))


  ## extract relevant structure
  DF <- as.data.table(ToDataFrameNetwork(tree$root,'p','T','oname'))
  if(length(quants)>0){
    for(qn in quants){
      tmp <- as.data.table(ToDataFrameNetwork(tree$root,'oname',qn))
      bname <- c('oname',qn)
      tmp <- tmp[,..bname]
      DF <- merge(DF,tmp,by='oname')
    }
  }
  tnmz <- unique(DF$T)                  #time names
  tnmz <- tnmz[tnmz!='0']
  ntnmz <- setdiff(pmz,tnmz)            #not time names
  hsh <- DF[,.(from,oname,op=p,oT=T)]
  DF <- merge(DF,
              hsh[,.(to=from,onameto=oname,op,oT)],
              by='to',all.x=TRUE)
  ## in rate strings
  DF[,inrates:=paste0(oname,fbrkts,'*(',op,')/(',T,fbrktsT,')')]

  ## make in rates in right place and initial
  DFU <- unique(DF[,.(oname)])
  DFU[,inits:=paste0('initial(',oname,brkts,') <- 0')]
  DFU <- merge(DFU,DF[T!=0,.(oname=onameto,inrates)],
               by='oname',all.x = TRUE)
  DFU[is.na(inrates),inrates:='0']
  DFU[oname==rootname,inrates:=rootinflow]

  ## add brackets to inrates
  if(L>0){
     for(pn in ntnmz){
      fs <- paste0("\\b",pn,"\\b")
      fr <- paste0(pn,fbrkts)
      DFU[,inrates:=gsub(fs,fr,inrates)]
    }
  }
  
  ## make outrates
  DF[,outrates:=paste0(oname,fbrkts,'/(',T,fbrktsT,')')]
  DFU <- merge(DFU,unique(DF[T!=0,.(oname,outrates)]),by='oname',all.x=TRUE)
  DFU[is.na(outrates),outrates:='0']


  ## finalize dynamics
  DFU[,dyx:=paste0('deriv(',oname,brkts,') <- ',inrates,' - ',outrates)]

  ## keep the tree path/names
  DFU <- merge(DFU,hsh[,.(oname,from)],by='oname',all.x=TRUE)

  ## dimz bit
  dmzbit <- NULL
  if(L>0){
    ## parameters
    for(pn in ntnmz){
      tmp <- paste0('dim(',pn,') <- ',dmz)
      dmzbit <- c(dmzbit,tmp)
    }
    if(times.array){
      for(pn in tnmz){
        tmp <- paste0('dim(',pn,') <- ',dmz)
        dmzbit <- c(dmzbit,tmp)
      }
    }
    ## variables
    for(vn in DFU[,unique(oname)]){
        tmp <- paste0('dim(',vn,') <- ',dmz)
        dmzbit <- c(dmzbit,tmp)
    }
  }
  
  ## input bit
  imp1 <- paste0(ntnmz,brkts,' <- user()')
  if(times.array){
    imp2 <- paste0(tnmz,brkts,' <- user()')
  } else {
      imp2 <- paste0(tnmz,' <- user()')
  }
  imp <- c(imp1,imp2)

  ## quantities calculator 
  ## NOTE also needs to be array
  qntz <- NULL
  if(length(quants)>0){
    quantsL <- c('oname',quants)
    DFU <- merge(DFU,DF[,..quantsL],by='oname')
    for(qn in quants){
      qnmz <- unlist(unique(DFU[,..qn]))
      ditch <- grepl("^[0-9]+$", qnmz, perl = TRUE)
      qnmz <- qnmz[!ditch]
      tmp1 <- NULL
      tmp2 <- paste0('initial(',qn,brkts,') <- 0' )
      tmp3a <- paste0('deriv(',qn,brkts,') <- ')
      tmp3b <- unlist(DFU[,..qn])
      tmp3c <- paste0('* ',DFU[,inrates])
      tmp3 <- paste0(tmp3a,paste(tmp3b,tmp3c,collapse = '+'))
      if(L>0){
        tmp4 <- paste0('dim(',qn,') <- ',dmz )
      } else {tmp4 <- NULL;}
      ## declare cost vars 
      if(length(qnmz)>0){               #NOTE could introduce brackets/dims
        tmp5 <- paste0(qnmz,'<- user()')
      } else {tmp5 <- NULL;}
      tmp <- c(tmp1,tmp2,tmp3,tmp4,tmp5)
      qntz <- c(qntz,tmp)
    }
  }

  ## make odin equations
  EQNS <- c(DFU$inits,DFU$dyx)
  EQNS <- c(imp,EQNS) #add in parameters
  EQNS <- c(EQNS,dmzbit)                         #dimension statements
  EQNS <- c(EQNS,qntz)                         #aux quantities

  ## return value
  list(odata=(DFU),pmz=pmz,EQNS=EQNS)
}

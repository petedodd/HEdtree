##' @title Making Odin ODEs from tree
##' @param tree 
##' @param rootname
##' @param rootinflow
##' @return list 
##' @author Pete Dodd
##' @export 
makeOdinModel <- function(tree,rootname='N1',rootinflow='0'){
  ## add aliased names: should work by side-effect on tree
  tree$Set(oname=rootname)
  tree$Do(function(node) if(!node$isLeaf){
                         for(i in 1:node$count)
                           node$children[[i]]$oname <- paste(node$oname,i,
                                                             sep='.')
                         })

  ## extract relevant structure
  DF <- as.data.table(ToDataFrameNetwork(tree$root,'p','T','oname'))
  hsh <- DF[,.(from,oname,op=p,oT=T)]
  DF <- merge(DF,
              hsh[,.(to=from,onameto=oname,op,oT)],
              by='to',all.x=TRUE)
  ## in rate strings
  DF[,inrates:=paste0(oname,'*(',op,')/(',T,')')]

  ## make in rates in right place and initial
  DFU <- unique(DF[,.(oname)])
  DFU[,inits:=paste0('initial(',oname,') <- 0')]
  DFU <- merge(DFU,DF[T!=0,.(oname=onameto,inrates)],
               by='oname',all.x = TRUE)
  DFU[is.na(inrates),inrates:='0']
  DFU[oname==rootname,inrates:=rootinflow]

  ## make outrates
  DF[,outrates:=paste0(oname,'/(',T,')')]
  DFU <- merge(DFU,unique(DF[T!=0,.(oname,outrates)]),by='oname',all.x=TRUE)
  DFU[is.na(outrates),outrates:='0']

  ## finalize dynamics
  DFU[,dyx:=paste0('deriv(',oname,') <- ',inrates,' - ',outrates)]

  print(DFU[,.(oname,inits,inrates,outrates,dyx)])

  ## grab parameters
  tmp <- showParmz(tree,parmz=c('p','T'))
  pmz <- tmp$vars
  pmz <- pmz[pmz!='0']
  pmz <- pmz[pmz!='1']
  tmp <- tmp$calcs
  tmp <- unique(unlist(strsplit(tmp,split="-|/|\\*|\\+"))) #find variables in calculations
  ditch <- grepl("^[0-9]+$", tmp, perl = TRUE)
  tmp <- tmp[!ditch]
  pmz <- unique(c(pmz,tmp))

  ## keep the tree path/names
  DFU <- merge(DFU,hsh[,.(oname,from)],by='oname',all.x=TRUE)

  ## make odin equations
  EQNS <- c(DFU$inits,DFU$dyx)
  EQNS <- c(paste0(pmz,' <- user()'),EQNS) #add in parameters

  ## return value
  list(odata=(DFU),pmz=pmz,EQNS=EQNS)
}
